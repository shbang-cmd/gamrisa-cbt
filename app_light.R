# 감리사 기출 CBT - 문제별 영구 메모 + 이미지 캐시 (고정 answer_key.csv 사용)
# 사용: app.R, answer_key.csv, 연도별 PDF를 같은 폴더에 둔 뒤 shiny::runApp()
# 권장: 2015년_CBT_정답표시제거.pdf 같은 '정답표시제거' PDF를 같은 폴더에 두면
#       앱이 원본보다 우선 사용하여, 문제 화면에서 굵은 정답 표시를 숨깁니다.

required <- c('shiny', 'pdftools', 'magick')
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

# 현재 PDF / 전체 연도 학습 위치를 각각 저장하는 책갈피
bookmark_path <- file.path(app_dir, 'cbt_bookmarks.rds')
read_bookmarks <- function() {
  if (!file.exists(bookmark_path)) return(list())
  x <- tryCatch(readRDS(bookmark_path), error=function(e) list())
  if (!is.list(x)) list() else x
}
write_bookmark <- function(scope, subject, year, number) {
  stopifnot(scope %in% c('local', 'global'), subject %in% names(subject_ranges))
  data <- read_bookmarks()
  data[[scope]] <- list(subject=as.character(subject),
                        year=as.character(year), number=as.integer(number),
                        saved_at=Sys.time())
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
      .memo-editor {width:100%;min-height:520px;resize:vertical;font-size:14px;line-height:1.6;border:1px solid #bcc6d0;border-radius:5px;padding:10px;}
      .memo-status {font-size:12px;color:#67717c;margin-top:8px;}
      .difficulty-row {display:flex;gap:7px;flex-wrap:wrap;margin:9px 0;}
      .difficulty-row .btn {flex:1 1 26%;padding:8px 5px;}
      .difficulty-help {font-size:12px;color:#67717c;margin:7px 0 12px;}
      .review-btn {width:100%;margin:5px 0 14px;}
      @media(max-width:900px){.study-layout{flex-direction:column;}.study-question,.study-memo{width:100%;flex:auto;}.study-memo{position:static;}}
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
    document.addEventListener('focusout',function(e){
      if(e.target.classList.contains('memo-editor'))sendMemo(e.target);
    });
    document.addEventListener('click',function(e){
      if(e.target.classList.contains('qimg'))e.target.classList.toggle('enlarged');
      if(e.target.id==='memo_save_btn')sendMemo(document.querySelector('.memo-editor'));
    });"))
  ),
  titlePanel('정보시스템감리사 기출 CBT (2015~2025)'),
  p('정답 제거 PDF 선택 후 불러오면, 저장된 120개 이미지가 있으면 재사용하고 없으면 새로 생성합니다.'),
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
      textOutput('subject_status'),
      hr(),
      uiOutput('jump_ui'),
      actionButton('random_btn', '무작위 문제'),
      hr(), strong('학습 현황'), verbatimTextOutput('summary'),
      downloadButton('save_csv', '풀이 결과 CSV 다운로드'),
      actionButton('clear_btn', '답안 초기화')
    ),
    mainPanel(uiOutput('main_ui'))
  )
)

server <- function(input, output, session) {
  questions <- reactiveVal(NULL)
  local_questions <- reactiveVal(NULL)
  current <- reactiveVal(1L)
  active_subject <- reactiveVal('all')
  active_scope <- reactiveVal('local')
  review_only <- reactiveVal(FALSE)
  difficulty_version <- reactiveVal(0L)
  bookmark_version <- reactiveVal(0L)
  startup_restoring <- reactiveVal(TRUE)
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
      write_bookmark(scope, active_subject(), q$year[current()], q$number[current()])
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
         year = as.character(q$year[i]), number = as.integer(q$number[i]))
  })
  delayed_position <- shiny::debounce(position_to_save, millis = 600)
  observeEvent(delayed_position(), {
    pos <- delayed_position()
    if (is.null(pos)) return()
    tryCatch({
      write_bookmark(pos$scope, pos$subject, pos$year, pos$number)
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
    current(i)
    finished(FALSE)
    image_version(image_version() + 1L)
  })
  subject_indices <- reactive({
    q <- questions()
    if (is.null(q)) return(integer(0))
    ids <- which(q$number %in% subject_ranges[[active_subject()]])
    if (review_only() && length(ids)) {
      difficulty_version()
      saved <- read_difficulties()
      ids <- ids[vapply(ids, function(i) {
        val <- saved[[memo_key(q$year[i], q$number[i])]]
        is.character(val) && length(val) == 1L && val %in% c('medium', 'hard')
      }, logical(1))]
    }
    ids
  })
  output$subject_status <- renderText({
    scope_name <- if (active_scope() == 'global') '전체 연도 이미지' else '현재 PDF'
    paste0('현재 모드: ', scope_name, ' / ', subject_names[[active_subject()]],
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
        review_only(FALSE)
        active_subject(id)
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
        review_only(FALSE)
        active_subject(id)
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
  observeEvent(input$check_btn, { q <- questions(); if (is.null(q)) return(); v <- checked(); v[current()] <- TRUE; checked(v) })
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
          uiOutput('feedback')
        ),
        div(class='study-memo',
          uiOutput('difficulty_ui'),
          tags$hr(),
          h4('문제별 학습 메모'),
          p(class='text-muted', sprintf('%s년 %d번 전용', q$year[n], q$number[n])),
          tags$textarea(memo_content, class='memo-editor', id='memo_text',
            `data-year`=as.character(q$year[n]),
            `data-number`=as.character(q$number[n]),
            placeholder='개념 정리, 오답 원인, AI 해설을 기록해 보세요.'),
          tags$button(id='memo_save_btn', type='button', class='btn btn-default', '지금 저장'),
          div(class='memo-status', '입력 후 약 0.6초에 자동 저장 · 문제 이동 시에도 저장'),
          div(class='memo-status', '저장 파일: cbt_memos.rds')
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
                           status=q$status, difficulty=levels),
                file, row.names=FALSE, fileEncoding='UTF-8')
    }
  )
}

shinyApp(ui=ui, server=server)
