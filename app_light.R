# 감리사 기출 CBT - 문제별 영구 메모 + 이미지 캐시 (고정 answer_key.csv 사용)
# 사용: app.R, answer_key.csv, 연도별 PDF를 같은 폴더에 둔 뒤 shiny::runApp()
# 권장: 2015년_CBT_정답표시제거.pdf 같은 '정답표시제거' PDF를 같은 폴더에 두면
#       앱이 원본보다 우선 사용하여, 문제 화면에서 굵은 정답 표시를 숨깁니다.

required <- c('shiny', 'pdftools', 'magick', 'markdown')
missing_pkgs <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0L) {
  stop(paste0('패키지 설치가 필요합니다: install.packages(c(',
              paste(sprintf('"%s"', missing_pkgs), collapse = ', '), '))'))
}
library(shiny)

# Windows: Shiny가 시작될 때 Chrome을 최대화된 새 창으로 엽니다.
# Chrome이 없거나 Windows가 아닌 경우에는 기본 브라우저를 사용합니다.
# RStudio Viewer가 아닌 외부 브라우저로 열도록 실행 옵션을 설정합니다.
options(shiny.launch.browser = function(url) {
  if (.Platform$OS.type == "windows") {
    chrome_paths <- c(
      file.path(Sys.getenv("PROGRAMFILES"), "Google", "Chrome", "Application", "chrome.exe"),
      file.path(Sys.getenv("PROGRAMFILES(X86)"), "Google", "Chrome", "Application", "chrome.exe"),
      file.path(Sys.getenv("LOCALAPPDATA"), "Google", "Chrome", "Application", "chrome.exe")
    )
    chrome_paths <- chrome_paths[file.exists(chrome_paths)]
    if (length(chrome_paths) > 0L) {
      launched <- tryCatch({
        system2(chrome_paths[1],
                args = c("--new-window", "--start-maximized", shQuote(url)),
                wait = FALSE)
        TRUE
      }, error = function(e) FALSE)
      if (launched) return(invisible(NULL))
    }
  }
  utils::browseURL(url)
  invisible(NULL)
})

app_dir <- getwd()
out_dir <- file.path(app_dir, 'www', 'auto_questions')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

circle_labels <- c('①','②','③','④')

# 연도에 공통 적용하는 문항별 과목 구간
subject_ranges <- list(
  all = 1:120,
  audit = 1:25,
  software = 26:50,
  database = 51:75,
  system = 76:100,
  security = 101:120
)
subject_names <- c(all='전체 문제', audit='감리 및 사업관리',
                   software='소프트웨어공학', database='데이터베이스',
                   system='시스템구조', security='보안')

# 세부 영역 분류표: PDF 내용을 기반으로 만든 잠정 분류. 오분류 가능성이 있어 CSV 편집 가능.
domain_path <- file.path(app_dir, 'question_domains.csv')
domain_groups <- list(
  audit = c('법령 및 제도','감리 절차 및 기준','사업관리·PMBOK','계약 및 발주','품질·위험관리','미분류'),
  software = c('요구사항·분석','설계·아키텍처','개발방법론·프로세스','테스트·품질','유지보수·형상·추정','미분류'),
  database = c('데이터 모델링·정규화','SQL·질의 최적화','트랜잭션·동시성·복구','인덱스·저장구조·성능','분산·NoSQL·빅데이터','미분류'),
  system = c('컴퓨터구조·하드웨어','운영체제·프로세스','네트워크·통신','분산·클라우드·가상화','시스템성능·가용성','미분류'),
  security = c('암호학·키 관리','인증·접근통제','네트워크·시스템 보안','취약점·공격·대응','보안관리·법령·거버넌스','미분류')
)
all_domain_names <- unique(unlist(domain_groups, use.names=FALSE))
read_domains <- function() {
  if (!file.exists(domain_path)) return(data.frame(year=character(0),number=integer(0),domain=character(0)))
  x <- tryCatch(read.csv(domain_path, fileEncoding='UTF-8-BOM', stringsAsFactors=FALSE), error=function(e) NULL)
  if (is.null(x) || !all(c('year','number','domain') %in% names(x))) return(data.frame(year=character(0),number=integer(0),domain=character(0)))
  x$year <- as.character(x$year); x$number <- suppressWarnings(as.integer(x$number))
  x[!duplicated(paste(x$year,x$number)),,drop=FALSE]
}

# 현재 PDF / 전체 연도 학습 위치를 각각 저장하는 책갈피
# 영역별 문항 수: CSV의 실제 분류 건수를 메뉴 레이블에 반영
# 내부 선택값은 영역명 그대로 두므로 기존 검색, 복습, 북마크 동작은 유지됨
domain_choices <- function(subject_id) {
  if (!(subject_id %in% names(domain_groups))) return(character(0))
  domains <- domain_groups[[subject_id]]
  x <- read_domains()
  if (!('subject' %in% names(x))) return(stats::setNames(domains, paste0(domains, ' (0)')))
  x <- x[x$subject == subject_names[[subject_id]], , drop=FALSE]
  counts <- table(factor(x$domain, levels=domains))
  stats::setNames(domains, sprintf('%s (%d)', domains, as.integer(counts)))
}

# 5과목 선행개념 학습 경로 (CSV 순서). 난이도는 잠정 추정입니다.
learning_path_file <- file.path(app_dir, 'all_subject_learning_paths.csv')
read_learning_path <- function() {
  if (!file.exists(learning_path_file)) return(NULL)
  x <- tryCatch(read.csv(learning_path_file, fileEncoding='UTF-8-BOM',
                         stringsAsFactors=FALSE), error=function(e) NULL)
  if (is.null(x) || !all(c('year','number','subject','learning_order','chapter','concept','level') %in% names(x))) return(NULL)
  x$year <- as.character(x$year)
  x$number <- as.integer(x$number)
  x <- x[order(x$subject, as.integer(x$learning_order)),,drop=FALSE]
  x[!duplicated(paste(x$year,x$number)),,drop=FALSE]
}

bookmark_path <- file.path(app_dir, 'cbt_bookmarks.rds')
read_bookmarks <- function() {
  if (!file.exists(bookmark_path)) return(list())
  x <- tryCatch(readRDS(bookmark_path), error=function(e) list())
  if (!is.list(x)) list() else x
}
write_bookmark <- function(scope, subject, year, number, domain='') {
  stopifnot(scope %in% c('local', 'global'), subject %in% names(subject_ranges))
  data <- read_bookmarks()
  data[[scope]] <- list(subject=as.character(subject),
                        year=as.character(year), number=as.integer(number),
                        domain=as.character(domain), saved_at=Sys.time())
  temp <- tempfile('bookmarks_', tmpdir=app_dir, fileext='.rds')
  on.exit(if (file.exists(temp)) unlink(temp), add=TRUE)
  saveRDS(data, temp)
  if (!file.copy(temp, bookmark_path, overwrite=TRUE))
    stop('책갈피 저장 실패: 폴더 쓰기 권한을 확인하세요.')
  invisible(data)
}


# 메모는 (연도, 문제번호)를 키로 app_light.R 옆에 저장됩니다.
# PDF 및 이미지 캐시와 무관하게 보존됩니다.
memo_path <- file.path(app_dir, 'cbt_memos.rds')
read_memos <- function() {
  if (!file.exists(memo_path)) return(list())
  x <- tryCatch(readRDS(memo_path), error = function(e) {
    warning(paste('메모 파일을 읽지 못했습니다:', conditionMessage(e)))
    list()
  })
  if (!is.list(x)) return(list())
  x
}
memo_key <- function(year, number) paste0(year, '_', sprintf('%03d', as.integer(number)))
write_memo <- function(year, number, value) {
  if (!grepl('^20[0-9]{2}$', as.character(year)) ||
      is.na(number) || number < 1L || number > 120L) return(FALSE)
  key <- memo_key(year, number)
  notes <- read_memos()
  notes[[key]] <- as.character(value)
  tmp <- tempfile(pattern = 'cbt_memo_', tmpdir = app_dir, fileext = '.rds')
  on.exit(if (file.exists(tmp)) unlink(tmp), add=TRUE)
  saveRDS(notes, tmp)
  # Windows에서는 대상 파일이 존재할 때 rename이 실패할 수 있습니다.
  # 먼저 복사로 갱신하며 실패 시 에러를 호출자에게 전달합니다.
  if (!file.copy(tmp, memo_path, overwrite=TRUE)) stop('메모 파일 저장 실패: 폴더 쓰기 권한을 확인하세요.')
  TRUE
}

# 난이도는 연도+문항번호별로 영구 저장됩니다 (공통 PDF/전체연도).
difficulty_path <- file.path(app_dir, 'cbt_difficulty.rds')
difficulty_levels <- c(easy='쉬움', medium='보통', hard='어려움')
read_difficulties <- function() {
  if (!file.exists(difficulty_path)) return(list())
  x <- tryCatch(readRDS(difficulty_path), error=function(e) list())
  if (is.list(x)) x else list()
}
write_difficulty <- function(year, number, level) {
  if (!(level %in% names(difficulty_levels)) ||
      !grepl('^20[0-9]{2}$', as.character(year)) ||
      is.na(number) || number < 1L || number > 120L) return(FALSE)
  data <- read_difficulties()
  data[[memo_key(year, number)]] <- level
  tmp <- tempfile('cbt_diff_', tmpdir=app_dir, fileext='.rds')
  on.exit(if (file.exists(tmp)) unlink(tmp), add=TRUE)
  saveRDS(data, tmp)
  if (!file.copy(tmp, difficulty_path, overwrite=TRUE))
    stop('난이도 저장 실패: 폴더 쓰기 권한을 확인하세요.')
  TRUE
}

# 학습 진도: 정답 확인을 한 고유 문제(연도+번호)를 영구 기록합니다.
# 앱/과목/연도 모드를 바꿔도 중복 집계하지 않습니다.
progress_path <- file.path(app_dir, 'cbt_study_progress.rds')
read_study_progress <- function() {
  if (!file.exists(progress_path)) return(character())
  x <- tryCatch(readRDS(progress_path), error=function(e) character())
  if (!is.character(x)) return(character())
  unique(x[grepl('^20(1[5-9]|2[0-5])_[0-9]{3}$', x)])
}
record_study_progress <- function(year, number) {
  k <- memo_key(year, number)
  old <- read_study_progress()
  if (k %in% old) return(FALSE)
  tmp <- tempfile('cbt_progress_', tmpdir=app_dir, fileext='.rds')
  on.exit(if (file.exists(tmp)) unlink(tmp), add=TRUE)
  saveRDS(c(old, k), tmp)
  if (!file.copy(tmp, progress_path, overwrite=TRUE))
    stop('학습 진도 저장 실패: 폴더 쓰기 권한을 확인하세요.')
  TRUE
}

# 학습 진도만 초기화합니다. 다른 RDS 학습 기록은 건드리지 않습니다.
reset_study_progress <- function() {
  tmp <- tempfile('cbt_progress_reset_', tmpdir=app_dir, fileext='.rds')
  on.exit(if (file.exists(tmp)) unlink(tmp), add=TRUE)
  saveRDS(character(), tmp)
  if (!file.copy(tmp, progress_path, overwrite=TRUE))
    stop('학습 진도 초기화 실패: 폴더 쓰기 권한을 확인하세요.')
  TRUE
}

is_correct_answer <- function(selected, keys) {
  vapply(seq_along(selected), function(i) {
    if (is.na(selected[i]) || is.na(keys[i]) || !nzchar(keys[i])) return(FALSE)
    as.character(selected[i]) %in% strsplit(keys[i], ',', fixed=TRUE)[[1]]
  }, logical(1))
}

format_answer_labels <- function(key) {
  if (is.na(key) || !nzchar(key)) return('미등록')
  nums <- suppressWarnings(as.integer(strsplit(key, ',', fixed=TRUE)[[1]]))
  paste(circle_labels[nums], collapse=', ')
}

find_question_positions <- function(pdf_file) {
  pages <- pdftools::pdf_data(pdf_file)
  sizes <- pdftools::pdf_pagesize(pdf_file)
  all <- list()
  for (p in seq_along(pages)) {
    d <- pages[[p]]
    if (!nrow(d)) next
    w <- as.numeric(sizes$width[p]); h <- as.numeric(sizes$height[p])
    landscape_compact <- w > 800 && h < 700
    anchors <- if (landscape_compact) c(33,218,454,639) else if (w > 650) c(57,375) else c(46,307)
    if (p == 1L && !landscape_compact) next
    txt <- trimws(as.character(d$text))
    # 56. 형태 외에 PDF에서 56 과 . 이 서로 다른 문자로 추출되는 경우 지원
    # 위치 검사는 아래의 열 시작 x좌표 검사로 다시 제한합니다.
    is_number <- grepl('^[0-9]{1,3}[.．]', txt)
    bare <- which(grepl('^[0-9]{1,3}$', txt))
    dots <- which(txt %in% c('.', '．'))
    for (k in bare) {
      if (!length(dots)) next
      nearby <- dots[
        d$x[dots] >= d$x[k] & d$x[dots] - d$x[k] < 24 &
        abs(d$y[dots] - d$y[k]) < 8
      ]
      if (length(nearby)) is_number[k] <- TRUE
    }
    if (!any(is_number)) next
    cand <- d[is_number, , drop=FALSE]
    cand$number <- suppressWarnings(as.integer(
      sub('[^0-9].*$', '', txt[is_number])
    ))
    cand$column <- vapply(cand$x, function(x) which.min(abs(x-anchors)), integer(1))
    good <- abs(cand$x-anchors[cand$column]) <= 16 & cand$number >= 1 & cand$number <= 120
    if (landscape_compact && p == 1L) good <- good & cand$column >= 3
    cand <- cand[good, , drop=FALSE]
    if (!nrow(cand)) next
    cand$page <- p
    all[[length(all)+1L]] <- cand[, c('number','page','column','x','y'), drop=FALSE]
  }
  if (!length(all)) stop('문항 시작 번호를 찾지 못했습니다. PDF 형식을 확인하세요.')
  pos <- do.call(rbind, all)
  pos <- pos[order(pos$page, pos$column, pos$y), , drop=FALSE]
  selected <- pos[0, , drop=FALSE]
  target <- 1L
  for (i in seq_len(nrow(pos))) {
    if (pos$number[i] == target) {
      selected <- rbind(selected, pos[i, , drop=FALSE])
      target <- target + 1L
    }
  }
  if (nrow(selected) != 120L) {
    missing <- setdiff(seq_len(120L), selected$number)
    next_expected <- nrow(selected) + 1L
    seen <- sort(unique(pos$number))
    stop(sprintf(
      '120문항 중 %d문항 인식. 최초 누락 후보: %d번. 전체 감지 번호 중 미발견: %s',
      nrow(selected), next_expected,
      if (length(setdiff(seq_len(120L), seen)))
        paste(setdiff(seq_len(120L), seen), collapse=', ') else '없음(순서 또는 중복 문제)'
    ))
  }
  rownames(selected) <- NULL
  selected
}

trim_question_bottom <- function(img, padding_px = 18L) {
  tryCatch({
    info <- magick::image_info(img)
    w <- as.integer(info$width[1]); h <- as.integer(info$height[1])
    if (!is.finite(w) || !is.finite(h) || w < 50L || h < 100L) return(img)

    dat <- magick::image_data(
      magick::image_convert(img, colorspace = 'gray'), channels = 'gray')
    dims <- dim(dat)
    if (length(dims) != 3L || dims[2] != w || dims[3] != h) return(img)
    pixels <- matrix(as.integer(dat), nrow = w, ncol = h)

    xleft <- max(1L, as.integer(w * 0.035))
    xright <- min(w, as.integer(w * 0.965))
    interior <- pixels[xleft:xright, , drop = FALSE]
    ink <- colSums(interior < 205L)
    threshold <- max(4L, as.integer(nrow(interior) * 0.0025))
    active <- which(ink >= threshold)
    if (length(active) < 4L) return(img)

    cut_at <- min(h, max(80L, max(active) + padding_px))
    gap_idx <- which(diff(active) >= 51L)
    if (length(gap_idx)) {
      for (j in gap_idx) {
        before_y <- active[j]
        after_y <- active[j + 1L]
        if (before_y < h * 0.22) next
        ink_before <- sum(ink[seq_len(before_y)])
        ink_after <- sum(ink[after_y:h])
        if (ink_before > 0 && ink_after / ink_before < 0.025) {
          cut_at <- min(cut_at, min(h, before_y + padding_px))
          break
        }
      }
    }
    if (h - cut_at < 22L) return(img)
    magick::image_crop(img,
                       geometry = sprintf('%dx%d+0+0', w, cut_at),
                       repage = TRUE)
  }, error = function(e) img)
}

create_question_images <- function(pdf_file, target_dir, progress = NULL) {
  dir.create(target_dir, recursive=TRUE, showWarnings=FALSE)
  pos <- find_question_positions(pdf_file)
  total <- nrow(pos)
  if (total != 120L) stop(sprintf('120문항 중 %d개 감지했습니다.', total))

  page_w <- 595; page_h <- 842
  sizes <- tryCatch(pdftools::pdf_pagesize(pdf_file), error = function(e) NULL)
  current_page <- NA_integer_
  rendered <- NULL

  for (i in seq_len(total)) {
    p <- pos$page[i]
    if (is.na(p) || p < 1L) stop(sprintf('문항 %d의 PDF 페이지 정보가 올바르지 않습니다.', i))
    if (is.na(current_page) || current_page != p) {
      tmp_pattern <- paste0(tempfile(pattern = 'cbt_page_'), '_%d.%s')
      converted <- suppressMessages(pdftools::pdf_convert(
        pdf_file, format = 'png', pages = p, dpi = 155,
        filenames = tmp_pattern, verbose = FALSE
      ))
      if (length(converted) < 1L || !file.exists(converted[1])) {
        stop(sprintf('PDF %d페이지를 PNG로 변환하지 못했습니다.', p))
      }
      rendered <- magick::image_read(converted[1])
      unlink(converted)
      current_page <- p
    }

    if (is.data.frame(sizes) && nrow(sizes) >= p) {
      page_w <- as.numeric(sizes$width[p]); page_h <- as.numeric(sizes$height[p])
    }
    image_dims <- magick::image_info(rendered)
    sx <- image_dims$width[1] / page_w
    sy <- image_dims$height[1] / page_h

    column <- pos$column[i]
    compact <- page_w > 800 && page_h < 700
    if (compact) {
      lefts <- c(27,212,448,633)
      rights <- c(211,393,632,page_w-22)
      top_margin <- 23; bottom_margin <- 27
    } else if (page_w > 650) {
      lefts <- c(46,364); rights <- c(355,page_w-38)
      top_margin <- 48; bottom_margin <- 42
    } else {
      lefts <- c(37,299); rights <- c(294,page_w-27)
      top_margin <- 45; bottom_margin <- 45
    }
    x0 <- lefts[column]; x1 <- rights[column]
    y0 <- max(top_margin, pos$y[i] - 9)
    same_col <- which(pos$page == p & pos$column == column & pos$y > pos$y[i] + 3)
    # y1 <- if (length(same_col) > 0L) min(pos$y[same_col]) - 7 else page_h - bottom_margin
    y1 <- if (length(same_col) > 0L) min(pos$y[same_col]) - 3 else page_h - bottom_margin  # 자르는 여백 조정
    if (y1 <= y0 + 20) y1 <- y0 + 75
    y1 <- min(page_h - 10, y1)

    px <- round(x0 * sx); py <- round(y0 * sy)
    pw <- max(1L, round((x1-x0) * sx)); ph <- max(1L, round((y1-y0) * sy))
    geom <- sprintf('%dx%d+%d+%d', pw, ph, px, py)
    filename <- sprintf('q%03d.png', pos$number[i])
    # 이미지를 실제로 생성합니다. 하단 자동 트리밍은 보기 잘림을 방지하기 위해 생략합니다.
    question_img <- magick::image_crop(rendered, geometry = geom, repage = TRUE)
    magick::image_write(question_img, file.path(target_dir, filename), format='png')
    if (!is.null(progress)) progress(i / total, sprintf('%d / %d 문항', i, total))
  }
  data.frame(number=pos$number, image=sprintf('q%03d.png', pos$number), stringsAsFactors=FALSE)
}

# 캐시 확인: 120개가 모두 존재하고, PDF 지문이 같으면 재사용합니다.
question_manifest <- function(target_dir) {
  file.path(target_dir, sprintf('q%03d.png', 1:120))
}

question_index <- function() {
  data.frame(number = 1:120,
             image = sprintf('q%03d.png', 1:120), stringsAsFactors = FALSE)
}

get_cached_questions <- function(pdf_file, target_dir, force = FALSE) {
  if (force || !all(file.exists(question_manifest(target_dir)))) return(NULL)
  meta_path <- file.path(target_dir, 'cache_meta.rds')
  signature <- unname(tools::md5sum(pdf_file))
  if (file.exists(meta_path)) {
    meta <- tryCatch(readRDS(meta_path), error = function(e) NULL)
    if (is.null(meta) || !identical(meta$pdf_md5, signature)) return(NULL)
  } else {
    # 예전 버전에서 잘라 놓은 이미지도 그대로 재사용합니다.
    # 기존 이미지에 PDF 메타데이터가 없으므로, 현재 PDF와 같다는 보장은 없습니다.
    message('기존 120개 이미지 재사용: PDF 원본 지문이 기록되지 않은 이전 캐시입니다.')
    return(list(questions = question_index(), legacy = TRUE))
  }
  list(questions = question_index(), legacy = FALSE)
}

build_or_reuse_questions <- function(pdf_file, target_dir, force = FALSE, progress = NULL) {
  cache <- get_cached_questions(pdf_file, target_dir, force = force)
  if (!is.null(cache)) {
    return(list(questions = cache$questions,
                message = if (cache$legacy) '기존 이미지 재사용(원본 PDF 일치 여부 미검증)' else '기존 이미지 재사용'))
  }
  # 재생성 중 에러가 나도 기존 120장에 잘못된 캐시 메타데이터가 남지 않게 합니다.
  meta_path <- file.path(target_dir, 'cache_meta.rds')
  if (file.exists(meta_path)) unlink(meta_path)
  pos <- create_question_images(pdf_file, target_dir, progress = progress)
  if (!all(file.exists(question_manifest(target_dir)))) stop('일부 문제 이미지가 생성되지 않았습니다.')
  saveRDS(list(pdf_md5 = unname(tools::md5sum(pdf_file))), meta_path)
  list(questions = pos, message = if (force) '문제 이미지 강제 재생성 완료' else '문제 이미지 최초 생성 완료')
}

# fixed answer key
key_path <- file.path(app_dir, 'answer_key.csv')
if (!file.exists(key_path)) stop('app.R과 같은 폴더에 answer_key.csv를 넣어 주세요.')
answer_db <- read.csv(key_path, stringsAsFactors=FALSE, fileEncoding='UTF-8-BOM',
                      colClasses='character', check.names=FALSE)
required_cols <- c('year','type','number','answer','status')
if (!all(required_cols %in% names(answer_db))) stop('answer_key.csv에 year,type,number,answer,status 열이 필요합니다.')
answer_db$year <- trimws(answer_db$year)
answer_db$type <- toupper(trimws(answer_db$type))
answer_db$number <- suppressWarnings(as.integer(answer_db$number))
if (anyDuplicated(answer_db[, c('year','type','number')])) stop('answer_key.csv에 중복된 연도/유형/문제번호가 있습니다.')

key_for_row <- function(answer, status) {
  status <- if (is.na(status)) '' else trimws(status)
  if (status %in% c('REVIEW','UNKNOWN') || !nzchar(status)) return(NA_character_)
  if (startsWith(status, 'MULTI:')) {
    nums <- strsplit(sub('^MULTI:', '', status), ',', fixed=TRUE)[[1]]
    nums <- trimws(nums)
    if (length(nums) >= 2L && all(nums %in% as.character(1:4))) return(paste(unique(nums), collapse=','))
    return(NA_character_)
  }
  if (identical(status, 'SINGLE') && !is.na(answer) && answer %in% as.character(1:4)) return(answer)
  NA_character_
}

get_clean_pdfs <- function() {
  pdfs <- list.files(app_dir, pattern='[.]pdf$', full.names=FALSE, ignore.case=TRUE)
  pdfs <- pdfs[grepl('정답표시제거|답제거', pdfs)]
  sort(pdfs)
}

# 이미 생성된 연도별 PNG만 수집합니다. PDF를 다시 변환하지 않습니다.
load_all_cached_images <- function() {
  dirs <- list.dirs(out_dir, recursive = FALSE, full.names = TRUE)
  results <- list()
  for (folder in dirs) {
    dirname <- basename(folder)
    if (!grepl('^20(1[5-9]|2[0-5])_A$', dirname)) next
    year <- substr(dirname, 1L, 4L)
    filenames <- list.files(folder, pattern = '^q[0-9]{3}\\.png$', full.names = FALSE)
    if (!length(filenames)) next
    nums <- suppressWarnings(as.integer(sub('^q([0-9]{3})\\.png$', '\\1', filenames)))
    keep <- !is.na(nums) & nums >= 1L & nums <= 120L
    nums <- nums[keep]; filenames <- filenames[keep]
    if (!length(nums)) next
    db <- answer_db[answer_db$year == year & answer_db$type == 'A', , drop = FALSE]
    hit <- match(nums, db$number)
    valid <- !is.na(hit)
    if (!any(valid)) next
    nums <- nums[valid]; filenames <- filenames[valid]; hit <- hit[valid]
    pos <- data.frame(number = nums, image = filenames,
                      answer = db$answer[hit], status = db$status[hit],
                      stringsAsFactors = FALSE)
    pos$status[is.na(pos$status)] <- 'UNKNOWN'
    pos$answer_keys <- vapply(seq_len(nrow(pos)), function(i)
      key_for_row(pos$answer[i], pos$status[i]), character(1))
    pos$image_url <- paste0('auto_questions/', dirname, '/', filenames)
    pos$year <- year
    pos$type <- 'A'
    results[[length(results) + 1L]] <- pos
  }
  if (!length(results)) return(NULL)
  all <- do.call(rbind, results)
  all <- all[order(-as.integer(all$year), all$number), , drop = FALSE]
  all <- all[!duplicated(paste(all$year, all$number)), , drop = FALSE]
  rownames(all) <- NULL
  all
}

get_pdf_year <- function(pdf_name) {
  m <- regmatches(basename(pdf_name), regexpr('20(1[5-9]|2[0-5])', basename(pdf_name)))
  if (length(m) == 0L || !nzchar(m)) return(NA_character_)
  m
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML('
      # .qwrap {text-align:center;margin:12px 0 18px;overflow:auto;max-height:66vh;}
      .qwrap {text-align:center;margin:12px 0 18px;overflow:auto;max-height:80vh;}
      .qimg {max-width:100%;height:auto;display:block;margin:auto;cursor:zoom-in;}
      .qimg.enlarged {max-width:none;max-height:none;}
      .actionbar {display:flex;flex-wrap:wrap;gap:8px;margin:12px 0;}
      .subject-buttons .btn {display:block;width:100%;margin:5px 0;text-align:left;white-space:normal;}
      .subject-columns {display:grid;grid-template-columns:minmax(0,1fr) minmax(0,1fr);gap:10px;}
      .subject-column {min-width:0;}
      .subject-column h5 {font-size:13px;font-weight:bold;line-height:1.4;min-height:36px;}
      .subject-column .btn {font-size:12px;padding:8px 5px;}
      .subject-columns .subject-column:last-child {border-left:1px solid #ddd;padding-left:10px;}
      .study-layout {display:flex;gap:18px;align-items:flex-start;}
      .study-question {flex:1 1 63%;min-width:0;}
      .study-memo {flex:0 0 34%;min-width:230px;position:sticky;top:12px;border:1px solid #d7dde4;border-radius:8px;padding:14px;background:#fafbfc;}
      .memo-title-row {display:flex;justify-content:space-between;align-items:center;gap:10px;flex-wrap:wrap;}
      .memo-title-row h4 {margin:0;}
      .memo-hide-label {font-size:13px;font-weight:normal;white-space:nowrap;cursor:pointer;}
      .memo-hide-label input {vertical-align:middle;margin:0 5px 0 0;}
      .study-memo.memo-hidden .memo-body {display:none;}
      .memo-hidden-notice {display:none;color:#66727d;font-size:13px;padding:22px 10px;text-align:center;border:1px dashed #ccd5df;border-radius:6px;background:#fff;margin-top:10px;}
      .study-memo.memo-hidden .memo-hidden-notice {display:block;}
      .memo-editor {width:100%;min-height:520px;resize:vertical;font-size:14px;line-height:1.6;border:1px solid #bcc6d0;border-radius:5px;padding:10px;}
      .memo-status {font-size:12px;color:#67717c;margin-top:8px;}
      .memo-preview-pane {cursor:text;}
      .memo-preview-hint {font-size:12px;color:#67717c;margin:9px 0 6px;}
      .memo-preview {min-height:520px;max-height:72vh;overflow:auto;border:1px solid #bcc6d0;border-radius:5px;padding:12px;background:white;line-height:1.7;overflow-wrap:anywhere;}
      .memo-preview table {border-collapse:collapse;max-width:100%;}
      .memo-preview th,.memo-preview td {border:1px solid #ddd;padding:5px;}
      .memo-preview img {max-width:100%;}
      .memo-preview .MathJax_Display {overflow-x:auto;overflow-y:hidden;}

      .study-progress {margin:14px 0 6px;padding:13px 14px;border:1px solid #d9e1e8;border-radius:9px;background:#fbfcfe;}
      .study-progress-title {font-weight:700;font-size:14px;margin-bottom:10px;}
      .study-progress-header {display:grid;grid-template-columns:94px minmax(0,1fr);align-items:center;gap:8px;margin-bottom:10px;}
      .study-progress-reset {font-size:11px;padding:5px 4px;width:94px;}
      .study-progress-header .study-progress-title {margin-bottom:0;}
      .study-progress-row {display:grid;grid-template-columns:94px minmax(0,1fr) 98px;align-items:center;gap:8px;margin:9px 0;}
      .study-progress-name {font-size:12px;font-weight:600;}
      .study-progress-track {height:16px;border-radius:9px;background:#e7ecf1;overflow:hidden;box-shadow:inset 0 1px 2px #ccd4df;}
      .study-progress-fill {height:100%;border-radius:9px;min-width:0;transition:width .35s ease;}
      .study-progress-count {text-align:right;font-size:12px;color:#485568;font-variant-numeric:tabular-nums;white-space:nowrap;}
      .study-progress-note {font-size:11px;color:#697687;margin-top:10px;}
      @media(max-width:600px){.study-progress-row{grid-template-columns:72px minmax(0,1fr) 82px;gap:5px;}.study-progress-name,.study-progress-count{font-size:11px;}}
      .difficulty-row {display:flex;gap:7px;flex-wrap:wrap;margin:9px 0;}
      .difficulty-row .btn {flex:1 1 26%;padding:8px 5px;}
      .difficulty-help {font-size:12px;color:#67717c;margin:7px 0 12px;}
      .review-btn {width:100%;margin:5px 0 14px;}
      @media(max-width:900px){.study-layout{flex-direction:column;}.study-question,.study-memo{width:100%;flex:auto;}.study-memo{position:static;}}

      /* Excel-style focus view: cosmetic only; the Shiny application stays active. */
      .cbt-view-toolbar {display:flex;align-items:center;gap:10px;margin:10px 0 16px;}
      .cbt-view-toolbar .btn {border-color:#197347;}
      .cbt-view-hint {font-size:12px;color:#60716a;}
      .normal-zoom-control {display:inline-flex;align-items:center;gap:6px;margin-left:10px;white-space:nowrap;font-size:13px;}
      .normal-zoom-control button {padding:4px 9px;border-radius:4px;border:1px solid #c0c9d0;background:#fff;cursor:pointer;}
      .normal-zoom-control button:disabled {opacity:.4;cursor:default;}
      .normal-zoom-control .normal-zoom-label {display:inline-block;min-width:45px;text-align:center;font-weight:600;}
      body:not(.excel-mode) .qimg:not(.enlarged) {zoom:var(--normal-question-scale, 1);}
      body.excel-mode .normal-zoom-control {display:none !important;}
      #excel_shell, #excel_footer {display:none;}
      body.excel-mode {background:#f5f7f5;font-family:Arial,"Malgun Gothic",sans-serif;}
      body.excel-mode .container-fluid {padding-left:0;padding-right:0;}
      body.excel-mode #cbt_normal_header, body.excel-mode .container-fluid > .row > .col-sm-4,
      body.excel-mode .cbt-view-hint {display:none !important;}
      body.excel-mode #excel_shell,body.excel-mode #excel_footer {display:block;}
      body.excel-mode .cbt-view-toolbar {position:absolute;right:18px;top:7px;margin:0;z-index:10;}
      body.excel-mode #excel_toggle {background:#ffffff;color:#176a43;border:1px solid #dadada;font-size:12px;padding:5px 11px;}
      .excel-title {height:49px;background:#217346;color:white;display:flex;align-items:center;padding:0 16px;gap:15px;font-size:14px;font-weight:600;}
      .excel-tile {background:#165b3b;padding:5px 9px;border-radius:3px;font-weight:bold;}
      .excel-menu {height:38px;background:#fff;display:flex;align-items:center;gap:29px;padding:0 18px;border-bottom:1px solid #dce1de;font-size:13px;}
      .excel-ribbon {height:64px;background:#fafafa;border-bottom:1px solid #d5dcd6;display:flex;align-items:center;gap:22px;padding:8px 20px;color:#4e5a52;font-size:12px;}
      .excel-tool {border-right:1px solid #d6ddd6;padding-right:20px;}
      .excel-formula {height:37px;background:#fff;display:flex;align-items:center;gap:14px;padding:0 14px;border-bottom:1px solid #d8ddd9;font-size:12px;}
      .excel-name-box {width:65px;border:1px solid #cdd5d0;text-align:center;padding:4px 2px;}
      .excel-fx {font-style:italic;color:#768179;}
      body.excel-mode .container-fluid > .row > .col-sm-8 {width:100% !important;float:none !important;margin:0 !important;padding:16px 35px 25px 62px;min-height:67vh;
        background-color:white;background-image:linear-gradient(to right,#e9eee9 1px,transparent 1px),linear-gradient(to bottom,#e9eee9 1px,transparent 1px);
        background-size:97px 28px;}
      body.excel-mode .study-memo,body.excel-mode .study-progress,
      body.excel-mode .study-question>.text-muted {display:none !important;}
      body.excel-mode .study-layout {display:block;}
      body.excel-mode .study-question {width:100%;max-width:1050px;background:#fff;padding:12px 20px;border:1px solid #cad9cb;box-shadow:0 2px 6px #00000012;}
      body.excel-mode #main_ui>h3 {font-size:16px;font-weight:600;color:#246b49;background:#fff;margin:2px 0 0;padding:12px 20px;max-width:1050px;border:1px solid #cad9cb;border-bottom:0;}
      /* Spreadsheet grid underneath the white PNG: multiply blending makes white pixels reveal grid lines,
         while dark question text remains dark. Original PNG files are never modified. */
      body.excel-mode .study-question {background-color:#fff;
        background-image:linear-gradient(to right,#e3eae4 1px,transparent 1px),linear-gradient(to bottom,#e3eae4 1px,transparent 1px);
        background-size:92px 26px;box-shadow:none;border:1px solid #bbd7c3;}
      body.excel-mode .qwrap {margin:5px 0 12px;max-height:60vh;text-align:left;
        background-color:white;
        background-image:linear-gradient(to right,#dfe8e1 1px,transparent 1px),linear-gradient(to bottom,#dfe8e1 1px,transparent 1px);
        background-size:92px 26px;}
      body.excel-mode .qwrap .qimg {mix-blend-mode:multiply;}
      body.excel-mode .container-fluid > .row > .col-sm-8 {padding-bottom:105px;}
      .excel-column-headers {display:flex;height:25px;border-bottom:1px solid #d9e2dc;background:#f7f8f7;color:#5c6660;font-size:11px;overflow:hidden;}
      .excel-column-headers span {box-sizing:border-box;width:92px;flex:0 0 92px;text-align:center;padding:4px 0;border-right:1px solid #dce4df;}
      .excel-column-headers span:first-child {width:44px;flex-basis:44px;background:#ecefec;}
      body.excel-mode #excel_footer {position:fixed;bottom:0;left:0;right:0;z-index:30;box-shadow:0 -2px 5px #00000009;}
      .excel-tabs {height:34px;display:flex;align-items:stretch;background:#f4f6f4;border-top:1px solid #cdd9d0;padding-left:10px;gap:0;}
      .excel-tabs .excel-tab-nav {font-size:16px;color:#63726a;padding:7px 11px;}
      .excel-tabs .excel-tab {display:flex;align-items:center;padding:0 17px;border-right:1px solid #dbe2db;color:#5c6c60;font-size:12px;white-space:nowrap;}
      .excel-tabs .excel-tab.active {background:#fff;color:#186b43;border-bottom:3px solid #217346;font-weight:700;}
      .excel-tabs .excel-tab-add {font-size:18px;padding:5px 12px;color:#6f7e73;}
      .excel-statusbar {height:30px !important;}
      .excel-sheet {background:transparent !important;border:0 !important;padding:0 !important;font-weight:normal !important;color:inherit !important;}

      body.excel-mode .qimg:not(.enlarged) {margin:0;zoom:var(--excel-question-scale, .9);max-width:100%;} /* 엑셀 모드: 10% 단위 확대/축소 */
      .excel-zoom-control {display:flex;align-items:center;gap:7px;margin-left:auto;white-space:nowrap;}
      .excel-zoom-control button {background:#fff;border:1px solid #b7c6bb;border-radius:3px;padding:5px 10px;color:#245e3c;font-weight:bold;cursor:pointer;}
      .excel-zoom-control button:disabled {opacity:.35;cursor:default;}
      .excel-zoom-control .excel-zoom-label {display:inline-block;min-width:44px;text-align:center;font-weight:bold;}
      body.excel-mode .shiny-input-radiogroup {margin:0 0 7px;}
      body.excel-mode .shiny-input-radiogroup>label {font-size:12px;color:#526159;}
      body.excel-mode .actionbar {margin:8px 0 12px;gap:6px;}
      body.excel-mode .actionbar .btn {background:#f7faf7;color:#25583d;border:1px solid #a6bbaa;border-radius:2px;font-size:12px;}
      body.excel-mode .actionbar #check_btn {background:#217346;color:white;}
      body.excel-mode #feedback .alert {padding:7px 12px;font-size:12px;}
      .excel-statusbar {height:37px;border-top:1px solid #ced8cf;background:#f7f9f7;display:flex;align-items:center;justify-content:space-between;padding:0 20px;color:#53655a;font-size:12px;}
      .excel-sheet {background:white;border-bottom:2px solid #217346;padding:9px 18px;color:#217346;font-weight:600;}
      @media(max-width:700px){.excel-ribbon{height:auto;gap:8px;flex-wrap:wrap;}.excel-menu{gap:12px;}.excel-title{font-size:12px;}body.excel-mode .container-fluid > .row > .col-sm-8{padding:12px 8px;}body.excel-mode .cbt-view-toolbar{right:8px;}}

    ')),
    tags$script(HTML("document.addEventListener('keydown',function(e){
      if(e.repeat||e.ctrlKey||e.altKey||e.metaKey||e.isComposing)return;
      var tag=(e.target.tagName||'').toLowerCase();
      if(tag==='textarea'||tag==='select'||e.target.isContentEditable)return;
      if(tag==='input'&&e.target.type!=='radio')return;
      var k=e.key.toLowerCase();
      if(['1','2','3','4'].includes(k)){
        var radio=document.querySelector('input[name=selected][value='+k+']');
        if(radio){e.preventDefault();radio.click();}
      } else {
        var id={p:'prev_btn',c:'check_btn',n:'next_btn'}[k];
        if(id){var b=document.getElementById(id);if(b){e.preventDefault();b.click();}}
      }
    });
    // UI-only scale: modify only the Excel-mode question image, not the Shiny server state.
    var excelZoom=90;
    function updateExcelZoom(){
      document.documentElement.style.setProperty('--excel-question-scale',String(excelZoom/100));
      var label=document.getElementById('excel_zoom_value');
      if(label)label.textContent=excelZoom+'%';
      var smaller=document.getElementById('excel_zoom_out');
      var larger=document.getElementById('excel_zoom_in');
      if(smaller)smaller.disabled=excelZoom<=40;
      if(larger)larger.disabled=excelZoom>=100;
    }
    document.addEventListener('click',function(e){
      var id=e.target && e.target.id;
      if(id==='excel_zoom_out'||id==='excel_zoom_in'){
        excelZoom=Math.max(40,Math.min(100,excelZoom+(id==='excel_zoom_out'?-10:10)));
        updateExcelZoom();
      }
    });
    document.addEventListener('DOMContentLoaded',updateExcelZoom);
    // Normal CBT image scaling (separate from Excel mode; persisted during question navigation).
    var normalZoom=100;
    function updateNormalZoom(){
      document.documentElement.style.setProperty('--normal-question-scale',String(normalZoom/100));
      var label=document.getElementById('normal_zoom_value');
      if(label)label.textContent=normalZoom+'%';
      var smaller=document.getElementById('normal_zoom_out');
      var larger=document.getElementById('normal_zoom_in');
      if(smaller)smaller.disabled=normalZoom<=40;
      if(larger)larger.disabled=normalZoom>=150;
    }
    document.addEventListener('click',function(e){
      var id=e.target && e.target.id;
      if(id==='normal_zoom_out'||id==='normal_zoom_in'){
        normalZoom=Math.max(40,Math.min(150,normalZoom+(id==='normal_zoom_out'?-10:10)));
        updateNormalZoom();
      }
    });
    document.addEventListener('DOMContentLoaded',updateNormalZoom);
    // CSS-only switch: retain the current question, selected answer and all server state.
    function setExcelMode(enabled){
      document.body.classList.toggle('excel-mode',enabled);
      var btn=document.getElementById('excel_toggle');
      if(btn){btn.textContent=enabled?'일반 CBT로 복귀':'엑셀 위장 전환';btn.setAttribute('aria-pressed',String(enabled));}
    }
    document.addEventListener('click',function(e){
      if(e.target && e.target.id==='excel_toggle')setExcelMode(!document.body.classList.contains('excel-mode'));
    });

    // 메모는 현재 입력 중인 문제의 key와 함께 전송하여 문제 이동 시 섞이지 않게 합니다.
    var memoTimer=null;
    function sendMemo(el){
      if(!el || !el.classList.contains('memo-editor') || !window.Shiny)return;
      if(memoTimer){clearTimeout(memoTimer);memoTimer=null;}
      Shiny.setInputValue('memo_payload',{
        year:el.dataset.year, number:Number(el.dataset.number),
        text:el.value, nonce:Date.now()
      },{priority:'event'});
    }
    document.addEventListener('input',function(e){
      if(!e.target.classList.contains('memo-editor'))return;
      var el=e.target;
      if(memoTimer)clearTimeout(memoTimer);
      memoTimer=setTimeout(function(){sendMemo(el);},600);
    });
    // 체크박스는 문제를 넘겨도 유지합니다. 해설을 가려도 메모 원문은 수정하지 않습니다.
    var memoHidden = false;
    function updateMemoVisibility(){
      var panel=document.querySelector('.study-memo');
      var toggle=document.getElementById('memo_hide_toggle');
      if(!panel || !toggle)return;
      toggle.checked=memoHidden;
      panel.classList.toggle('memo-hidden',memoHidden);
    }
    document.addEventListener('change',function(e){
      if(e.target.id!=='memo_hide_toggle')return;
      memoHidden=e.target.checked;
      // 현재 편집 중인 내용도 숨기기 전에 저장합니다.
      if(memoHidden)sendMemo(document.querySelector('.memo-editor'));
      updateMemoVisibility();
      if(!memoHidden)showMemoMode('preview',true);
    });
    // 평소에는 수식 미리보기를 표시하고, 미리보기를 클릭하면 편집합니다.
    function showMemoMode(mode, requestPreview){
      var editor=document.querySelector('.memo-editor');
      var editing=document.getElementById('memo_edit_pane');
      var preview=document.getElementById('memo_preview_pane');
      if(!editor || !editing || !preview)return;
      editing.style.display=mode==='edit'?'':'none';
      preview.style.display=mode==='edit'?'none':'';
      if(mode==='preview' && requestPreview && window.Shiny){
        sendMemo(editor);
        Shiny.setInputValue('memo_preview_request',{
          year:editor.dataset.year,number:Number(editor.dataset.number),
          text:editor.value,nonce:Date.now()
        },{priority:'event'});
      }
      if(mode==='edit' && !memoHidden)editor.focus();
    }
    document.addEventListener('focusout',function(e){
      if(!e.target.classList.contains('memo-editor'))return;
      sendMemo(e.target);
      // 포커스를 다른 곳으로 옮기면 자동으로 수식 보기 모드로 복귀합니다.
      setTimeout(function(){
        if(document.activeElement!==e.target)showMemoMode('preview',true);
      },0);
    });
    document.addEventListener('click',function(e){
      if(!memoHidden && e.target.closest && e.target.closest('#memo_preview_pane')){
        showMemoMode('edit',false);
        return;
      }
      if(e.target.classList.contains('qimg'))e.target.classList.toggle('enlarged');
      if(e.target.id==='memo_save_btn')sendMemo(document.querySelector('.memo-editor'));
    });
    // 문제 전환·앱 시작마다 새로운 문제의 메모를 미리보기로 불러옵니다.
    $(document).on('shiny:value',function(e){
      if(e.target.id!=='main_ui')return;
      setTimeout(function(){updateMemoVisibility();showMemoMode('preview',!memoHidden);},100);
    });
    // Shiny가 미리보기 HTML을 새로 그릴 때 MathJax 수식을 다시 조판합니다.
    $(document).on('shiny:value',function(e){
      if(e.target.id!=='memo_preview_ui')return;
      setTimeout(function(){
        var target=document.getElementById('memo_preview_ui');
        if(!target)return;
        if(window.MathJax && MathJax.Hub && MathJax.Hub.Queue){
          MathJax.Hub.Queue(['Typeset',MathJax.Hub,target]);
        }else if(window.MathJax && MathJax.typesetPromise){
          MathJax.typesetPromise([target]).catch(function(){});
        }
      },60);
    });")),
    withMathJax()
  ),
  div(id='excel_shell',
    div(class='excel-title', span(class='excel-tile', 'X'),
        span('학습자료 관리 - 통합 문서'),
        span(style='margin-left:auto;margin-right:140px;font-size:12px;font-weight:normal;', '자동 저장')),
    div(class='excel-menu', span('파일'), span('홈'), span('삽입'), span('페이지 레이아웃'), span('수식'), span('데이터'), span('검토')),
    div(class='excel-ribbon',
        span(class='excel-tool', '붙여넣기　 클립보드'),
        span(class='excel-tool', '글꼴　 11　 굵게'),
        span(class='excel-tool', '맞춤　 가운데'),
        span(class='excel-tool', '표시 형식　 일반'),
        div(class='excel-zoom-control',
            span('문제 크기'),
            tags$button(id='excel_zoom_out', type='button', `aria-label`='문제 크기 10% 축소', '−10%'),
            span(id='excel_zoom_value', class='excel-zoom-label', '90%'),
            tags$button(id='excel_zoom_in', type='button', `aria-label`='문제 크기 10% 확대', '+10%'))),
    div(class='excel-formula',span(class='excel-name-box','B8'),span(class='excel-fx','fx'),span('기출문제 학습 내용')),
    div(class='excel-column-headers', lapply(c('',LETTERS[1:14]), function(x) span(x)))),
  div(class='cbt-view-toolbar',
      tags$button(id='excel_toggle',type='button',class='btn btn-default',
                  `aria-pressed`='false','엑셀 위장 전환'),
      span(class='cbt-view-hint','화면 형태만 변경 · 문제 위치와 답안 유지'),
      div(class='normal-zoom-control',
          span('문제 크기'),
          tags$button(id='normal_zoom_out', type='button', `aria-label`='일반 CBT 문제 10% 축소', '−10%'),
          span(id='normal_zoom_value', class='normal-zoom-label', '100%'),
          tags$button(id='normal_zoom_in', type='button', `aria-label`='일반 CBT 문제 10% 확대', '+10%'))),
  div(id='cbt_normal_header',
      titlePanel('정보시스템감리사 기출 CBT (2015~2025)'),
      p('정답 제거 PDF 선택 후 불러오면, 저장된 120개 이미지가 있으면 재사용하고 없으면 새로 생성합니다.')),
  sidebarLayout(
    sidebarPanel(
      selectInput('pdf_file', '① 정답 제거 PDF 선택',
                  choices=c('PDF를 선택하세요'='', stats::setNames(get_clean_pdfs(), get_clean_pdfs())),
                  selected=''),
      textOutput('chosen_year'),
      actionButton('build', 'CBT 문제 불러오기', class='btn-primary'),
      actionButton('rebuild', '문제 이미지 다시 생성'),
      hr(), textOutput('build_status'), hr(),
      strong('과목별 문제풀이'),
      div(class='subject-columns',
        div(class='subject-column subject-buttons',
          h5('현재 PDF에서 과목별'),
          actionButton('local_all', '전체 문제'),
          actionButton('local_audit', '감리 및 사업관리'),
          actionButton('local_software', '소프트웨어공학'),
          actionButton('local_database', '데이터베이스'),
          actionButton('local_system', '시스템구조'),
          actionButton('local_security', '보안'),
          hr(),
          strong('학습 책갈피'),
          textOutput('local_bookmark_info'),
          actionButton('local_bookmark_save', '현재 위치 저장'),
          actionButton('local_bookmark_resume', '이어 풀기')
        ),
        div(class='subject-column subject-buttons',
          h5('전체 연도에서 과목별'),
          actionButton('global_all', '전체 문제'),
          actionButton('global_audit', '감리 및 사업관리'),
          actionButton('global_software', '소프트웨어공학'),
          actionButton('global_database', '데이터베이스'),
          actionButton('global_system', '시스템구조'),
          actionButton('global_security', '보안'),
          hr(),
          strong('학습 책갈피'),
          textOutput('global_bookmark_info'),
          actionButton('global_bookmark_save', '현재 위치 저장'),
          actionButton('global_bookmark_resume', '이어 풀기')
        )
      ),
      hr(),
      strong('5과목 · 독학 학습 경로'),
      p('선행 개념 → 기초 → 응용 순서 (자동 잠정 분류, 추가 확인 문항은 맨 뒤)'),
      selectInput('learning_subject', '학습 경로 과목',
                  choices=stats::setNames(c('audit','software','database','system','security'),
                                          subject_names[c('audit','software','database','system','security')]),
                  selected='software'),
      actionButton('start_learning_path', '선택 과목 학습 경로 시작', class='btn-success'),
      actionButton('stop_learning_path', '학습 경로 종료'),
      textOutput('learning_path_status'),
      hr(),
      strong('과목별 세부 영역(Domain) 문제풀이'),
      selectInput('domain_subject', '과목', choices=stats::setNames(names(domain_groups), subject_names[names(domain_groups)]), selected='audit'),
      selectInput('domain_select', '세부 영역', choices=domain_choices('audit'), selected='법령 및 제도'),
      div(class='subject-columns',
        div(class='subject-column', actionButton('domain_local','현재 PDF 영역 풀이',class='btn-info')),
        div(class='subject-column', actionButton('domain_global','전체 연도 영역 풀이',class='btn-primary'))
      ),
      actionButton('domain_clear', '세부 영역 해제'),
      textOutput('domain_status'),
      textOutput('subject_status'),
      hr(),
      uiOutput('jump_ui'),
      actionButton('random_btn', '무작위 문제'),
      hr(), strong('학습 현황'), verbatimTextOutput('summary'),
      downloadButton('save_csv', '풀이 결과 CSV 다운로드'),
      actionButton('clear_btn', '답안 초기화')
    ),
    mainPanel(uiOutput('main_ui'))
  ),
  div(id='excel_footer',
      div(class='excel-tabs',
          span(class='excel-tab-nav','‹　›'),
          span(class='excel-tab active','학습자료'),
          span(class='excel-tab','문항목록'),
          span(class='excel-tab','정답기록'),
          span(class='excel-tab','진도현황'),
          span(class='excel-tab-add','+')),
      div(class='excel-statusbar',
          span(class='excel-sheet','준비'),
          span('P 이전 · 1~4 답 선택 · C 채점 · N 다음　 |　 Excel 스타일')))
)

server <- function(input, output, session) {
  questions <- reactiveVal(NULL)
  local_questions <- reactiveVal(NULL)
  current <- reactiveVal(1L)
  active_subject <- reactiveVal('all')
  active_scope <- reactiveVal('local')
  active_domain <- reactiveVal('')
  learning_path_mode <- reactiveVal(FALSE)
  review_only <- reactiveVal(FALSE)
  difficulty_version <- reactiveVal(0L)
  bookmark_version <- reactiveVal(0L)
  study_progress_version <- reactiveVal(0L)
  startup_restoring <- reactiveVal(TRUE)
  observeEvent(input$reset_study_progress_btn, {
    showModal(modalDialog(
      title='학습 진도율을 초기화할까요?',
      '5과목 전체의 완료 문항 기록을 0개로 초기화합니다. 기존 메모, 난이도, 북마크 및 답안 기록은 그대로 유지됩니다.',
      footer=tagList(
        modalButton('취소'),
        actionButton('confirm_reset_study_progress', '진도율 초기화', class='btn-danger')
      ),
      easyClose=TRUE
    ))
  })
  observeEvent(input$confirm_reset_study_progress, {
    removeModal()
    tryCatch({
      reset_study_progress()
      study_progress_version(study_progress_version() + 1L)
      showNotification('5과목 학습 진도율을 0%로 초기화했습니다.', type='message')
    }, error=function(e) {
      showNotification(paste('진도율 초기화 오류:', conditionMessage(e)), type='error')
    })
  })

  bookmark_status <- function(scope) {
    bookmark_version()
    b <- read_bookmarks()[[scope]]
    if (is.null(b) || is.null(b$subject) || is.null(b$year) || is.null(b$number))
      return('저장된 위치 없음')
    sprintf('%s · %s년 %d번', subject_names[[b$subject]], b$year, b$number)
  }
  output$local_bookmark_info <- renderText(bookmark_status('local'))
  output$global_bookmark_info <- renderText(bookmark_status('global'))
  save_current_position <- function(scope, notify=FALSE) {
    q <- questions()
    if (is.null(q) || active_scope() != scope || current() < 1L || current() > nrow(q)) {
      if (notify) showNotification('해당 열의 문제를 먼저 선택해 주세요.', type='warning')
      return(invisible(FALSE))
    }
    tryCatch({
      write_bookmark(scope, active_subject(), q$year[current()], q$number[current()], active_domain())
      bookmark_version(bookmark_version() + 1L)
      if (notify) showNotification('학습 위치를 저장했습니다.', type='message')
      TRUE
    }, error=function(e) {
      showNotification(paste('책갈피 저장 오류:', conditionMessage(e)), type='error')
      FALSE
    })
  }
  # 문제 화면에 0.6초 이상 머물면 마지막 학습 위치를 자동 저장합니다.
  # Reactive 값에 연도와 문제번호를 함께 담아, 빠르게 이동한 이전 문제로
  # 책갈피가 뒤늦게 덮어써지지 않도록 합니다.
  position_to_save <- reactive({
    if (startup_restoring()) return(NULL)
    q <- questions()
    if (is.null(q)) return(NULL)
    i <- current()
    if (i < 1L || i > nrow(q)) return(NULL)
    list(scope = active_scope(), subject = active_subject(),
         year = as.character(q$year[i]), number = as.integer(q$number[i]), domain=active_domain())
  })
  delayed_position <- shiny::debounce(position_to_save, millis = 600)
  observeEvent(delayed_position(), {
    pos <- delayed_position()
    if (is.null(pos)) return()
    tryCatch({
      write_bookmark(pos$scope, pos$subject, pos$year, pos$number, pos$domain)
      bookmark_version(bookmark_version() + 1L)
    }, error = function(e) {
      showNotification(paste('자동 책갈피 저장 오류:', conditionMessage(e)),
                       type = 'error')
    })
  }, ignoreInit = FALSE)

  observeEvent(input$local_bookmark_save, save_current_position('local', TRUE))
  observeEvent(input$global_bookmark_save, save_current_position('global', TRUE))
  observeEvent(input$local_bookmark_resume, {
    b <- read_bookmarks()[['local']]
    if (is.null(b)) { showNotification('현재 PDF 책갈피가 없습니다.', type='warning'); return() }
    q <- local_questions()
    if (is.null(q) || !identical(as.character(q$year[1]), as.character(b$year))) {
      showNotification(paste0('먼저 ', b$year, '년 PDF를 불러온 뒤 이어 풀기를 누르세요.'), type='warning', duration=8)
      return()
    }
    i <- which(q$year == b$year & q$number == b$number)[1]
    if (is.na(i)) { showNotification('저장된 문제를 현재 PDF에서 찾지 못했습니다.', type='warning'); return() }
    if (active_scope() != 'local') {
      questions(q)
      picked(rep(NA_integer_, nrow(q)))
      checked(rep(FALSE, nrow(q)))
    }
    active_scope('local')
    active_subject(b$subject)
    active_domain(if (!is.null(b$domain) && b$domain %in% all_domain_names) b$domain else '')
    current(i)
    finished(FALSE)
  })
  observeEvent(input$global_bookmark_resume, {
    b <- read_bookmarks()[['global']]
    if (is.null(b)) { showNotification('전체 연도 책갈피가 없습니다.', type='warning'); return() }
    q <- load_all_cached_images()
    if (is.null(q)) { showNotification('전체 연도 캐시 이미지가 없습니다.', type='warning'); return() }
    i <- which(q$year == b$year & q$number == b$number)[1]
    if (is.na(i)) { showNotification('책갈피 문제의 이미지가 없습니다.', type='warning'); return() }
    questions(q)
    picked(rep(NA_integer_, nrow(q)))
    checked(rep(FALSE, nrow(q)))
    active_scope('global')
    active_subject(b$subject)
    active_domain(if (!is.null(b$domain) && b$domain %in% all_domain_names) b$domain else '')
    current(i)
    finished(FALSE)
    image_version(image_version() + 1L)
  })
  subject_indices <- reactive({
    q <- questions()
    if (is.null(q)) return(integer(0))
    ids <- which(q$number %in% subject_ranges[[active_subject()]])
    if (nzchar(active_domain()) && length(ids)) {
      domain_data <- read_domains()
      key <- paste(q$year[ids],q$number[ids])
      domain_value <- domain_data$domain[match(key,paste(domain_data$year,domain_data$number))]
      ids <- ids[!is.na(domain_value) & domain_value == active_domain()]
    }
    if (review_only() && length(ids)) {
      difficulty_version()
      saved <- read_difficulties()
      ids <- ids[vapply(ids, function(i) {
        val <- saved[[memo_key(q$year[i], q$number[i])]]
        is.character(val) && length(val) == 1L && val %in% c('medium', 'hard')
      }, logical(1))]
    }
    if (learning_path_mode() && length(ids)) {
      plan <- read_learning_path()
      if (!is.null(plan)) {
        plan <- plan[plan$subject == subject_names[[active_subject()]], , drop=FALSE]
        keys <- paste(q$year[ids], q$number[ids])
        ord <- plan$learning_order[match(keys, paste(plan$year,plan$number))]
        ids <- ids[order(is.na(ord), ord)]
      }
    }
    ids
  })
  output$learning_path_status <- renderText({
    if (!learning_path_mode()) return('학습 경로: 사용 안 함')
    q <- questions(); plan <- read_learning_path()
    if (is.null(q) || is.null(plan)) return('학습 경로 자료 없음')
    plan <- plan[plan$subject == subject_names[[active_subject()]], , drop=FALSE]
    i <- current()
    ix <- match(paste(q$year[i],q$number[i]),paste(plan$year,plan$number))
    if (is.na(ix)) return('현재 문항의 학습 단계 정보 없음')
    sprintf('%d/%d · %s → %s · %s (분류 신뢰도: %s)',
      match(i,subject_indices()), length(subject_indices()),
      plan$chapter[ix],plan$concept[ix],plan$level[ix],plan$confidence[ix])
  })
  observeEvent(input$start_learning_path, {
    plan <- read_learning_path()
    if (is.null(plan) || !nrow(plan)) {
      showNotification('all_subject_learning_paths.csv 파일을 찾지 못했습니다.',type='warning'); return()
    }
    q <- load_all_cached_images()
    if (is.null(q)) {
      showNotification('www/auto_questions 아래 문제 이미지가 필요합니다.',type='warning'); return()
    }
    id <- input$learning_subject
    if (is.null(id) || !(id %in% c('audit','software','database','system','security'))) return()
    if (!any(plan$subject == subject_names[[id]])) {
      showNotification('선택 과목의 학습 순서표가 없습니다.', type='warning'); return()
    }
    questions(q); active_scope('global'); active_subject(id)
    active_domain(''); review_only(FALSE); learning_path_mode(TRUE)
    picked(rep(NA_integer_,nrow(q))); checked(rep(FALSE,nrow(q)))
    eligible <- subject_indices()
    if (!length(eligible)) {
      learning_path_mode(FALSE)
      showNotification('선택 과목에 저장된 문제 이미지가 없습니다.', type='warning'); return()
    }
    current(eligible[1])
    finished(FALSE); image_version(image_version()+1L)
  })
  observeEvent(input$stop_learning_path, { learning_path_mode(FALSE) })
  output$subject_status <- renderText({
    scope_name <- if (active_scope() == 'global') '전체 연도 이미지' else '현재 PDF'
    paste0('현재 모드: ', scope_name, ' / ', subject_names[[active_subject()]],
           if (nzchar(active_domain())) paste0(' / ', active_domain()) else '',
           if (review_only()) ' / 보통·어려움 복습' else '',
           ' (', length(subject_indices()), '문항)')
  })
  for (subject_id in names(subject_ranges)) {
    local({
      id <- subject_id
      observeEvent(input[[paste0('local_', id)]], {
        q <- local_questions()
        if (is.null(q)) {
          showNotification('먼저 현재 PDF를 불러오세요.', type = 'warning')
          return()
        }
        if (active_scope() != 'local') {
          questions(q)
          picked(rep(NA_integer_, nrow(q)))
          checked(rep(FALSE, nrow(q)))
        }
        active_scope('local')
        learning_path_mode(FALSE)
        review_only(FALSE)
        active_subject(id)
        active_domain('')
        eligible <- which(q$number %in% subject_ranges[[id]])
        if (length(eligible)) current(eligible[1])
        finished(FALSE)
      })
      observeEvent(input[[paste0('global_', id)]], {
        q <- load_all_cached_images()
        if (is.null(q)) {
          showNotification('www/auto_questions/ 아래에 연도별 문제 이미지가 없습니다.',
                           type = 'warning', duration = 8)
          return()
        }
        eligible <- which(q$number %in% subject_ranges[[id]])
        if (!length(eligible)) {
          showNotification('선택 과목의 저장된 이미지가 없습니다.', type = 'warning')
          return()
        }
        questions(q)
        active_scope('global')
        learning_path_mode(FALSE)
        review_only(FALSE)
        active_subject(id)
        active_domain('')
        picked(rep(NA_integer_, nrow(q)))
        checked(rep(FALSE, nrow(q)))
        current(eligible[1])
        finished(FALSE)
        image_version(image_version() + 1L)
        status(sprintf('전체 연도 캐시: %d개 이미지 검색, %s %d문항',
                       nrow(q), subject_names[[id]], length(eligible)))
      })
    })
  }

  observeEvent(input$domain_subject, {
    id <- input$domain_subject
    if (is.null(id) || !(id %in% names(domain_groups))) return()
    ch <- domain_choices(id)
    updateSelectInput(session, 'domain_select', choices=ch, selected=unname(ch[1]))
  }, ignoreInit=FALSE)

  observeEvent(list(active_domain(), active_subject()), {
    if (!nzchar(active_domain())) return()
    id <- active_subject()
    if (!(id %in% names(domain_groups))) return()
    updateSelectInput(session, 'domain_subject', selected=id)
    updateSelectInput(session, 'domain_select', selected=active_domain())
  }, ignoreInit=TRUE)

  output$domain_status <- renderText({
    if (!nzchar(active_domain())) return('세부 영역 필터: 사용 안 함')
    paste0('선택 영역: ', subject_names[[active_subject()]], ' / ', active_domain(), ' / ', length(subject_indices()), '문항 (현재 이용 가능한 이미지 기준)')
  })
  launch_domain <- function(scope) {
    id <- input$domain_subject
    d <- input$domain_select
    if (is.null(id) || !(id %in% names(domain_groups)) || is.null(d) || !(d %in% domain_groups[[id]])) return()
    q <- if (scope == 'global') load_all_cached_images() else local_questions()
    if (is.null(q)) {
      showNotification(if (scope == 'global') '전체 연도 이미지가 없습니다.' else '먼저 PDF를 불러오세요.',type='warning')
      return()
    }
    domain_data <- read_domains()
    ids <- which(q$number %in% subject_ranges[[id]])
    domains <- domain_data$domain[match(paste(q$year[ids], q$number[ids]),
                                       paste(domain_data$year, domain_data$number))]
    ids <- ids[!is.na(domains) & domains == d]
    if (!length(ids)) {
      showNotification('이 영역에 분류된 이용 가능한 문제 이미지가 없습니다.',type='warning',duration=7)
      return()
    }
    if (scope == 'global' || active_scope() != 'local') {
      questions(q); picked(rep(NA_integer_, nrow(q))); checked(rep(FALSE,nrow(q)))
    }
    active_scope(scope)
    active_subject(id)
    learning_path_mode(FALSE)
    review_only(FALSE)
    active_domain(d)
    current(ids[1])
    finished(FALSE)
    image_version(image_version()+1L)
  }
  observeEvent(input$domain_local, launch_domain('local'))
  observeEvent(input$domain_global, launch_domain('global'))
  observeEvent(input$domain_clear, {
    active_domain('')
    eligible <- subject_indices()
    if (length(eligible) && !(current() %in% eligible)) current(eligible[1])
  })

  picked <- reactiveVal(integer(0))
  checked <- reactiveVal(logical(0))
  finished <- reactiveVal(FALSE)
  image_version <- reactiveVal(0L)
  status <- reactiveVal('정답 제거 PDF를 선택한 뒤 문제를 불러오세요.')
  output$build_status <- renderText(status())

  # 과목 버튼에 실제 이용 가능한 문항 수를 표시합니다.
  # 현재 PDF: 현재 불러온 시험의 문항 / 전체 연도: 저장된 PNG 중 정답표와 연결되는 문항
  observe({
    local_q <- local_questions()
    image_version()  # 이미지 생성 또는 재생성 후에도 표시를 갱신
    global_q <- load_all_cached_images()
    for (id in names(subject_ranges)) {
      ids <- subject_ranges[[id]]
      local_n <- if (is.null(local_q)) 0L else sum(local_q$number %in% ids)
      global_n <- if (is.null(global_q)) 0L else sum(global_q$number %in% ids)
      updateActionButton(session, paste0('local_', id),
                         label = sprintf('%s (%d)', subject_names[[id]], local_n))
      updateActionButton(session, paste0('global_', id),
                         label = sprintf('%s (%d)', subject_names[[id]], global_n))
    }
  })


  output$chosen_year <- renderText({
    req(input$pdf_file)
    year <- get_pdf_year(input$pdf_file)
    if (is.na(year)) return('파일명에서 시험 연도를 확인할 수 없습니다.')
    paste0('자동 인식 연도: ', year, '년 (A형 정답)')
  })

  load_exam <- function(force = FALSE, pdf_override = NULL, resume_bookmark = NULL) {
    if (is.null(pdf_override)) req(input$pdf_file)
    selected_name <- basename(if (is.null(pdf_override)) input$pdf_file else pdf_override)
    if (!selected_name %in% get_clean_pdfs()) {
      showNotification('정답 제거 PDF를 선택해 주세요.', type='error')
      return()
    }
    year <- get_pdf_year(selected_name)
    if (is.na(year)) {
      showNotification('파일명에 2015~2025년 연도가 필요합니다.', type='error')
      return()
    }
    typ <- 'A'
    selected_pdf <- file.path(app_dir, selected_name)
    if (!file.exists(selected_pdf)) {
      showNotification('선택한 문제지 PDF를 찾지 못했습니다.', type='error')
      return()
    }
    db <- answer_db[answer_db$year == year & answer_db$type == typ, , drop=FALSE]
    if (!nrow(db)) {
      showNotification('해당 연도의 정답이 없습니다.', type='error')
      return()
    }
    target_dir <- file.path(out_dir, paste0(year, '_', typ))
    status(if (force) 'PDF에서 문제 이미지를 다시 생성하는 중입니다...' else '기존 문제 이미지 캐시를 확인하는 중입니다...')
    tryCatch({
      loaded <- withProgress(message=if (force) '문항 이미지 재생성' else '문항 이미지 불러오기', value=0, {
        build_or_reuse_questions(selected_pdf, target_dir, force = force,
          progress=function(value, detail) setProgress(value=value, detail=detail))
      })
      pos <- loaded$questions
      hit <- match(pos$number, db$number)
      pos$answer <- db$answer[hit]
      pos$status <- db$status[hit]
      pos$status[is.na(pos$status)] <- 'UNKNOWN'
      pos$answer_keys <- vapply(seq_len(nrow(pos)), function(i) key_for_row(pos$answer[i], pos$status[i]), character(1))
      pos$image_url <- paste0('auto_questions/', year, '_', typ, '/', pos$image)
      pos$year <- year; pos$type <- typ
      local_questions(pos)
      questions(pos)
      active_scope('local')
      picked(rep(NA_integer_, nrow(pos)))
      checked(rep(FALSE, nrow(pos)))
      subject_id <- if (!is.null(resume_bookmark) &&
                        resume_bookmark$subject %in% names(subject_ranges))
                      resume_bookmark$subject else active_subject()
      active_subject(subject_id)
      active_domain(if (!is.null(resume_bookmark$domain) && resume_bookmark$domain %in% all_domain_names) resume_bookmark$domain else '')
      eligible <- which(pos$number %in% subject_ranges[[subject_id]])
      resume_i <- if (!is.null(resume_bookmark)) {
        which(pos$year == as.character(resume_bookmark$year) &
              pos$number == as.integer(resume_bookmark$number))[1]
      } else NA_integer_
      current(if (length(resume_i) && !is.na(resume_i) && resume_i %in% eligible)
                resume_i else if (length(eligible)) eligible[1] else 1L)
      finished(FALSE)
      image_version(image_version() + 1L)
      missing_q <- setdiff(db$number, pos$number)
      status(sprintf('%s년 | %s | %s | 이미지 %d개 / 정답 확인 %d개 / 미확정 %d개%s',
        year, basename(selected_pdf), loaded$message, nrow(pos), sum(!is.na(pos$answer_keys)), sum(is.na(pos$answer_keys)),
        if (length(missing_q)) paste0(' | 미추출 문제 ', paste(missing_q, collapse=',')) else ''))
      showNotification('문제 불러오기 완료', type='message')
    }, error=function(e) {
      status(paste('오류:', conditionMessage(e)))
      showNotification(conditionMessage(e), type='error', duration=NULL)
    })
  }

  # 앱 최초 접속 시 두 책갈피 중 최근 저장된 위치로 자동 복원합니다.
  # 최신 위치가 '현재 PDF'라면 그 연도 PDF도 자동 불러옵니다.
  observeEvent(TRUE, {
    on.exit(startup_restoring(FALSE), add = TRUE)
    bookmarks <- read_bookmarks()
    options <- bookmarks[c('local', 'global')]
    options <- Filter(function(b) is.list(b) &&
      !is.null(b$year) && !is.null(b$number) &&
      !is.null(b$subject) && b$subject %in% names(subject_ranges), options)
    if (!length(options)) return()

    saved_time <- vapply(options, function(b) {
      if (is.null(b$saved_at)) return(-Inf)
      suppressWarnings(as.numeric(as.POSIXct(b$saved_at)))
    }, numeric(1))
    saved_time[is.na(saved_time)] <- -Inf
    scope <- names(options)[which.max(saved_time)]
    b <- options[[scope]]

    if (identical(scope, 'global')) {
      q <- load_all_cached_images()
      if (is.null(q)) return()
      i <- which(as.character(q$year) == as.character(b$year) &
                 q$number == as.integer(b$number))[1]
      if (is.na(i) || !(q$number[i] %in% subject_ranges[[b$subject]])) return()
      questions(q)
      picked(rep(NA_integer_, nrow(q)))
      checked(rep(FALSE, nrow(q)))
      active_scope('global')
      active_subject(b$subject)
    active_domain(if (!is.null(b$domain) && b$domain %in% all_domain_names) b$domain else '')
      current(i)
      finished(FALSE)
      image_version(image_version() + 1L)
      status(sprintf('최근 학습 자동 복원: 전체 연도 / %s / %s년 %d번',
                     subject_names[[b$subject]], b$year, b$number))
    } else {
      pdfs <- get_clean_pdfs()
      matches <- pdfs[vapply(pdfs, function(x)
        identical(get_pdf_year(x), as.character(b$year)), logical(1))]
      if (!length(matches)) return()
      updateSelectInput(session, 'pdf_file', selected = matches[1])
      load_exam(force = FALSE, pdf_override = matches[1], resume_bookmark = b)
    }
  }, once = TRUE)

  observeEvent(input$build, load_exam(FALSE))
  observeEvent(input$rebuild, load_exam(TRUE))

  output$jump_ui <- renderUI({
    q <- questions(); if (is.null(q)) return(NULL)
    eligible <- subject_indices()
    selectInput('jump', '문제 번호 이동',
                choices=stats::setNames(eligible, paste0(q$year[eligible], '년 ', q$number[eligible], '번')),
                selected=as.character(current()))
  })
  observeEvent(input$jump, {
    q <- questions(); if (is.null(q)) return()
    n <- suppressWarnings(as.integer(input$jump))
    if (!is.na(n) && n %in% subject_indices() && n != current()) {
      current(n)
    }
  }, ignoreInit=TRUE)
  observeEvent(current(), {
    q <- questions(); if (is.null(q)) return()
    updateSelectInput(session, 'jump', selected=as.character(current()))
    a <- picked()[current()]
    freezeReactiveValue(input, 'selected')
    updateRadioButtons(session, 'selected', selected=if (is.na(a)) character(0) else as.character(a))
  })
  observeEvent(input$selected, {
    q <- questions(); if (is.null(q)) return()
    a <- suppressWarnings(as.integer(input$selected))
    if (is.na(a) || !a %in% 1:4) return()
    v <- picked(); v[current()] <- a; picked(v)
  }, ignoreInit=TRUE)
  observeEvent(input$check_btn, {
    q <- questions(); if (is.null(q)) return()
    n <- current()
    v <- checked(); v[n] <- TRUE; checked(v)
    tryCatch({
      record_study_progress(q$year[n], q$number[n])
      study_progress_version(study_progress_version() + 1L)
    }, error=function(e) showNotification(paste('진도 저장 오류:', conditionMessage(e)), type='error'))
  })
  move <- function(delta) {
    eligible <- subject_indices()
    if (!length(eligible)) return()
    idx <- match(current(), eligible)
    if (is.na(idx)) idx <- 1L
    # 선택 과목 마지막 문항에서 다음을 누르면 첫 문제로 순환
    next_idx <- ((idx - 1L + delta) %% length(eligible)) + 1L
    current(eligible[next_idx])
  }
  observeEvent(input$prev_btn, move(-1L))
  observeEvent(input$next_btn, move(1L))
  observeEvent(input$random_btn, { eligible <- subject_indices(); if (length(eligible)) current(eligible[sample.int(length(eligible), 1L)]) })
  observeEvent(input$finish_btn, finished(TRUE))
  observeEvent(input$resume_btn, finished(FALSE))
  observeEvent(input$clear_btn, {
    q <- questions(); if (is.null(q)) return()
    picked(rep(NA_integer_, nrow(q))); checked(rep(FALSE, nrow(q)))
    finished(FALSE); eligible <- subject_indices(); current(if (length(eligible)) eligible[1] else 1L)
  })

  # 브라우저에서 전송한 (연도, 문제번호, 메모) 조합을 영구 저장합니다.
  # 문제 이동 후 늦게 도착한 전송도 payload에 기록된 원래 키로 저장합니다.
  observeEvent(input$memo_payload, {
    payload <- input$memo_payload
    if (is.null(payload$year) || is.null(payload$number) || is.null(payload$text)) return()
    tryCatch({
      write_memo(as.character(payload$year), as.integer(payload$number), payload$text)
      # 메모 표시를 갱신하는 리액티브 의존성을 만들지 않아 입력 도중 화면이 사라지지 않습니다.
    }, error=function(e) showNotification(paste('메모 저장 오류:', conditionMessage(e)), type='error', duration=NULL))
  })

  # 문제 바로 위 난이도 선택: 클릭 즉시 파일에 기록합니다.
  # 출력 영역을 메모장과 분리하여 난이도 선택 시 메모 입력 내용을 지우지 않습니다.
  output$difficulty_ui <- renderUI({
    q <- questions(); if (is.null(q) || finished()) return(NULL)
    n <- current()
    if (n < 1L || n > nrow(q)) return(NULL)
    difficulty_version()
    saved <- read_difficulties()
    level <- saved[[memo_key(q$year[n], q$number[n])]]
    if (is.null(level) || !(level %in% names(difficulty_levels))) level <- ''
    buttons <- lapply(names(difficulty_levels), function(value) {
      txt <- difficulty_levels[[value]]
      cls <- if (identical(level, value)) {
        switch(value, easy='btn-success', medium='btn-warning', hard='btn-danger')
      } else 'btn-default'
      tags$button(type='button', class=paste('btn', cls),
        onclick=sprintf("Shiny.setInputValue('difficulty_payload',{year:'%s',number:%d,level:'%s',nonce:Date.now()},{priority:'event'});",
                        as.character(q$year[n]), as.integer(q$number[n]), value), txt)
    })
    review_ids <- which(q$number %in% subject_ranges[[active_subject()]])
    count <- sum(vapply(review_ids, function(i) {
      val <- saved[[memo_key(q$year[i], q$number[i])]]
      is.character(val) && length(val) == 1L && val %in% c('medium','hard')
    }, logical(1)))
    tagList(
      h4('문제 난이도'),
      div(class='difficulty-row', buttons),
      div(class='difficulty-help', '클릭 즉시 자동 저장 · 쉬움은 복습 목록에서 제외'),
      actionButton('review_toggle',
        if (review_only()) '전체 문제로 돌아가기' else sprintf('보통·어려움만 복습 (%d)', count),
        class=if (review_only()) 'btn-primary review-btn' else 'btn-info review-btn'),
      if (review_only()) div(class='difficulty-help', sprintf('현재 과목 복습 대상 %d문항', count))
    )
  })

  observeEvent(input$difficulty_payload, {
    payload <- input$difficulty_payload
    if (is.null(payload$year) || is.null(payload$number) || is.null(payload$level)) return()
    tryCatch({
      if (write_difficulty(as.character(payload$year), as.integer(payload$number),
                           as.character(payload$level))) {
        difficulty_version(difficulty_version() + 1L)
        if (review_only()) {
          valid <- subject_indices()
          if (length(valid) && !(current() %in% valid)) current(valid[1])
          if (!length(valid)) review_only(FALSE)
        }
      }
    }, error=function(e) showNotification(paste('난이도 저장 오류:', conditionMessage(e)), type='error'))
  })

  observeEvent(input$review_toggle, {
    q <- questions(); if (is.null(q)) return()
    if (review_only()) {
      review_only(FALSE)
      return()
    }
    difficulty_version()
    saved <- read_difficulties()
    ids <- which(q$number %in% subject_ranges[[active_subject()]])
    ids <- ids[vapply(ids, function(i) {
      val <- saved[[memo_key(q$year[i], q$number[i])]]
      is.character(val) && length(val) == 1L && val %in% c('medium','hard')
    }, logical(1))]
    if (!length(ids)) {
      showNotification('보통 또는 어려움으로 표시한 문제가 없습니다.', type='warning')
      return()
    }
    review_only(TRUE)
    if (!(current() %in% ids)) current(ids[1])
    finished(FALSE)
  })

  # Gemini 복사 시 중복된 Markdown/LaTeX 이스케이프를 미리보기에서만 정리합니다.
  # 원본 메모(cbt_memos.rds)는 전혀 변경하지 않습니다.
  render_memo_markdown <- function(content) {
    normalized <- content
    # 예: \\(x\_{1}\\) -> \(x_{1}\), \*\*강조\*\* -> **강조**
    for (delimiter in c('(', ')', '[', ']')) {
      normalized <- gsub(paste0('\\\\', delimiter), paste0('\\', delimiter),
                         normalized, fixed=TRUE)
    }
    normalized <- gsub('\\*', '*', normalized, fixed=TRUE)
    normalized <- gsub('\\_', '_', normalized, fixed=TRUE)

    # Markdown 처리기가 수식의 역슬래시를 소모하지 않도록 임시 토큰으로 보호합니다.
    # R 4.0+ raw string: 역슬래시 중복 이스케이프 없이 PCRE 정규식을 작성합니다.
    # 괄호 균형 및 수식 구분자를 명시하여 invalid regular expression 오류를 방지합니다.
    math_pattern <- r"{(?s)\\\[(.*?)\\\]|\\\((.*?)\\\)|\$\$(.*?)\$\$|(?<!\$)\$(?!\$)[^\r\n$]+?\$(?!\$)}"
    m <- gregexpr(math_pattern, normalized, perl=TRUE)[[1]]
    expressions <- if (m[1] == -1L) character() else regmatches(normalized, list(m))[[1]]
    placeholders <- character(length(expressions))
    if (length(expressions)) {
      for (i in seq_along(expressions)) {
        token <- sprintf('CBTMATHPLACEHOLDER%05dEND', i)
        placeholders[i] <- token
        normalized <- sub(math_pattern, token, normalized, perl=TRUE)
      }
    }
    safe <- as.character(htmltools::htmlEscape(normalized))
    rendered <- markdown::markdownToHTML(text=safe, fragment.only=TRUE)
    # 일부 Gemini 복사 텍스트에서는 **강조** 표시가 Markdown 처리 후에도
    # 문자 그대로 남습니다. 안전하게 HTML 이스케이프한 문자열에 한해 보정합니다.
    # 이미 Markdown이 <strong> 태그로 변환한 내용은 건드리지 않습니다.
    rendered <- gsub(
      r"{(?:\\)?\*(?:\\)?\*([^*\r\n]+?)(?:\\)?\*(?:\\)?\*}",
      '<strong>\\1</strong>', rendered, perl=TRUE
    )
    for (i in seq_along(expressions)) {
      expr <- expressions[i]
      if (startsWith(expr, '$') && !startsWith(expr, '$$')) {
        expr <- paste0('\\(', substring(expr, 2L, nchar(expr)-1L), '\\)')
      }
      # HTML은 이스케이프하지만 LaTeX 역슬래시는 유지합니다.
      safe_math <- as.character(htmltools::htmlEscape(expr))
      rendered <- gsub(placeholders[i], safe_math, rendered, fixed=TRUE)
    }
    rendered
  }

  # 메모 포커스가 사라지거나 문제가 바뀔 때 미리보기를 갱신합니다.
  # HTML 태그는 먼저 이스케이프해 AI 답변에 들어간 HTML 실행을 막습니다.
  output$memo_preview_ui <- renderUI({
    req(input$memo_preview_request)
    request <- input$memo_preview_request
    q <- questions()
    if (is.null(q) || finished()) return(NULL)
    n <- current()
    if (!identical(as.character(request$year), as.character(q$year[n])) ||
        !identical(as.integer(request$number), as.integer(q$number[n]))) return(NULL)
    content <- as.character(request$text)
    if (!nzchar(trimws(content))) {
      return(div(class='memo-preview', '저장된 메모가 없습니다.'))
    }
    rendered <- render_memo_markdown(content)
    div(class='memo-preview', HTML(rendered))
  })

  # 현재 문제 필터와 무관한 전체 2015~2025년 학습 진도(5과목 총 1320문항)
  output$study_progress_ui <- renderUI({
    study_progress_version()
    keys <- read_study_progress()
    ids <- c('audit','software','database','system','security')
    names_short <- c('감리·사업관리','소프트웨어','데이터베이스','시스템구조','보안')
    palette <- c('#3586db','#10a68a','#ed9b31','#8065cb','#e26783')
    total <- vapply(ids, function(id) length(subject_ranges[[id]]) * 11L, integer(1))
    numbers <- suppressWarnings(as.integer(sub('^.*_', '', keys)))
    done <- vapply(ids, function(id) sum(!is.na(numbers) & numbers %in% subject_ranges[[id]]), integer(1))
    rows <- lapply(seq_along(ids), function(i) {
      percent <- if (total[i]) 100 * done[i] / total[i] else 0
      div(class='study-progress-row',
        span(class='study-progress-name', names_short[i]),
        div(class='study-progress-track', role='progressbar',
            `aria-label`=paste0(names_short[i], ' 학습 진도'),
            `aria-valuenow`=done[i], `aria-valuemin`='0', `aria-valuemax`=total[i],
            div(class='study-progress-fill',
                style=sprintf('width:%.2f%%;background:%s;',percent,palette[i]))),
        span(class='study-progress-count', sprintf('%d/%d (%.1f%%)',done[i],total[i],percent))
      )
    })
    div(class='study-progress',
      div(class='study-progress-header',
        actionButton('reset_study_progress_btn', '진도율 초기화',
                     class='btn-default study-progress-reset'),
        div(class='study-progress-title',
            sprintf('5과목 전체 학습 진도 · %d / %d문항 (%.1f%%)',
                    sum(done),sum(total),100*sum(done)/sum(total)))),
      rows,
      div(class='study-progress-note',
          '정답 확인을 누르면 1문항 완료 · 같은 문제 반복 학습은 중복 제외 · 자동 저장: cbt_study_progress.rds')
    )
  })

  output$main_ui <- renderUI({
    q <- questions()
    if (is.null(q)) {
      return(tagList(h4('정답 제거 PDF를 선택해 CBT를 시작하세요.'),
                     p('정답 PDF는 필요하지 않습니다. answer_key.csv의 고정 정답으로 채점합니다.')))
    }
    if (finished()) return(tagList(h3('최종 결과'), uiOutput('stats'), actionButton('resume_btn', '문제로 돌아가기')))
    n <- current()
    memo_content <- read_memos()[[memo_key(q$year[n], q$number[n])]]
    if (is.null(memo_content)) memo_content <- ''
    tagList(
      h3(sprintf('%s년 · %s · %d번 (%d / %d)', q$year[n],
                 subject_names[[active_subject()]], q$number[n],
                 match(n, subject_indices()), length(subject_indices()))),
      div(class='study-layout',
        div(class='study-question',
          div(class='qwrap', tags$img(class='qimg', alt=paste0(q$number[n], '번 문제'),
            src=paste0(q$image_url[n], '?v=', image_version()))),
          p(class='text-muted', '이미지 클릭: 확대/축소'),
          radioButtons('selected', '답안 선택', choices=stats::setNames(as.character(1:4), circle_labels),
            selected=if (is.na(picked()[n])) character(0) else as.character(picked()[n]), inline=TRUE),
          div(class='actionbar', actionButton('prev_btn', '이전 (P)'),
            actionButton('check_btn', '정답 확인 (C)', class='btn-primary'),
            actionButton('next_btn', '다음 (N)'), actionButton('finish_btn', '결과 보기', class='btn-success')),
          uiOutput('feedback'),
          uiOutput('study_progress_ui')
        ),
        div(class='study-memo',
          uiOutput('difficulty_ui'),
          tags$hr(),
          div(class='memo-title-row',
            h4('문제별 학습 메모'),
            tags$label(class='memo-hide-label',
              tags$input(id='memo_hide_toggle', type='checkbox'), '해설 숨기기')),
          div(class='memo-hidden-notice', '해설을 숨겼습니다. 문제를 푼 뒤 체크를 해제해 확인하세요.'),
          div(class='memo-body',
            p(class='text-muted', sprintf('%s년 %d번 전용', q$year[n], q$number[n])),
            div(class='memo-preview-hint', '미리보기를 클릭하면 편집 · 편집창 밖을 클릭하면 수식 미리보기'),
            div(id='memo_edit_pane', style='display:none;',
              tags$textarea(memo_content, class='memo-editor', id='memo_text',
                `data-year`=as.character(q$year[n]),
                `data-number`=as.character(q$number[n]),
                placeholder='Gemini 해설, Markdown, LaTeX 수식을 붙여넣으세요.')),
            div(id='memo_preview_pane', class='memo-preview-pane',
              uiOutput('memo_preview_ui')),
            tags$button(id='memo_save_btn', type='button', class='btn btn-default', '지금 저장'),
            div(class='memo-status', '입력 후 약 0.6초에 자동 저장 · 문제 이동 시에도 저장'),
            div(class='memo-status', '저장 파일: cbt_memos.rds'))
        )
      )
    )
  })

  output$feedback <- renderUI({
    q <- questions(); if (is.null(q) || finished()) return(NULL)
    n <- current(); if (!checked()[n]) return(NULL)
    a <- picked()[n]; key <- q$answer_keys[n]
    if (is.na(key)) return(div(class='alert alert-warning', '정답 미확정(REVIEW/UNKNOWN): 채점에서 제외합니다.'))
    if (is.na(a)) return(div(class='alert alert-info', paste('정답:', format_answer_labels(key), '(미응답)')))
    div(class='alert alert-info', strong(if (is_correct_answer(a, key)) '정답입니다!' else '오답입니다.'),
        p(paste('내 답:', circle_labels[a], '/ 정답:', format_answer_labels(key))))
  })

  output$summary <- renderText({
    q <- questions(); if (is.null(q)) return('불러온 시험이 없습니다.')
    eligible <- subject_indices(); a <- picked()[eligible]
    keys <- q$answer_keys[eligible]
    valid <- !is.na(a) & !is.na(keys)
    correct <- sum(is_correct_answer(a, keys) & valid)
    paste0('응답 ', sum(!is.na(a)), ' / ', length(eligible), '\n정답 ', correct, ' / 채점 ', sum(valid),
           '\n정답 미확정 ', sum(is.na(keys)))
  })

  output$stats <- renderUI({
    q <- questions(); req(q)
    eligible <- subject_indices(); a <- picked()[eligible]
    keys <- q$answer_keys[eligible]
    valid <- !is.na(a) & !is.na(keys)
    ok <- is_correct_answer(a, keys)
    right <- valid & ok; wrong <- valid & !ok
    tagList(
      h4(sprintf('정답률: %s', if (sum(valid)) sprintf('%.1f%%', 100 * sum(right) / sum(valid)) else '채점 대상 없음')),
      p(sprintf('응답 %d / %d · 정답 %d · 오답 %d · 미확정 문항 %d', sum(!is.na(a)), length(eligible), sum(right), sum(wrong), sum(is.na(keys)))),
      p('오답 문제: ', if (any(wrong)) paste0(q$year[eligible][wrong], '년 ', q$number[eligible][wrong], '번', collapse=', ') else '없음')
    )
  })

  output$save_csv <- downloadHandler(
    filename=function() {
      q <- questions()
      if (is.null(q)) 'cbt_result.csv' else paste0('cbt_', if (active_scope() == 'global') 'all_years' else q$year[1], '_', Sys.Date(), '.csv')
    },
    content=function(file) {
      q <- questions(); req(q)
      a <- picked(); valid <- !is.na(a) & !is.na(q$answer_keys)
      ok <- is_correct_answer(a, q$answer_keys)
      difficulties <- read_difficulties()
      levels <- vapply(seq_len(nrow(q)), function(i) {
        v <- difficulties[[memo_key(q$year[i], q$number[i])]]
        if (is.null(v) || !(v %in% names(difficulty_levels))) return('')
        unname(difficulty_levels[[v]])
      }, character(1))
      write.csv(data.frame(year=q$year, type=q$type, number=q$number,
                           selected=a, answer=q$answer_keys, correct=ifelse(valid, ok, NA),
                           status=q$status, difficulty=levels,
                           domain=read_domains()$domain[match(paste(q$year,q$number),
                             paste(read_domains()$year,read_domains()$number))]),
                file, row.names=FALSE, fileEncoding='UTF-8')
    }
  )
}

shinyApp(ui=ui, server=server)
