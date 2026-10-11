# 감리사 기출 CBT - 연도 선택형 (정답파일 answer_key.csv 사용)
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

app_dir <- getwd()
out_dir <- file.path(app_dir, 'www', 'auto_questions')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

circle_labels <- c('①','②','③','④')

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
    is_number <- grepl('^[0-9]{1,3}[.]', txt)
    if (!any(is_number)) next
    cand <- d[is_number, , drop=FALSE]
    cand$number <- as.integer(sub('[.].*$', '', txt[is_number]))
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
    stop(sprintf('120문항 중 %d문항만 인식했습니다. PDF 파일과 형식을 확인하세요.', nrow(selected)))
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
    y1 <- if (length(same_col) > 0L) min(pos$y[same_col]) - 7 else page_h - bottom_margin
    if (y1 <= y0 + 20) y1 <- y0 + 75
    y1 <- min(page_h - 10, y1)

    px <- round(x0 * sx); py <- round(y0 * sy)
    pw <- max(1L, round((x1-x0) * sx)); ph <- max(1L, round((y1-y0) * sy))
    geom <- sprintf('%dx%d+%d+%d', pw, ph, px, py)
    filename <- sprintf('q%03d.png', pos$number[i])
    question_img <- magick::image_crop(rendered, geometry = geom, repage = TRUE)
    question_img <- trim_question_bottom(question_img, padding_px = 16L)
    magick::image_write(question_img, file.path(target_dir, filename), format='png')
    if (!is.null(progress)) progress(i / total, sprintf('%d / %d 문항', i, total))
  }
  data.frame(number=pos$number, image=sprintf('q%03d.png', pos$number), stringsAsFactors=FALSE)
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

get_pdf_year <- function(pdf_name) {
  m <- regmatches(basename(pdf_name), regexpr('20(1[5-9]|2[0-5])', basename(pdf_name)))
  if (length(m) == 0L || !nzchar(m)) return(NA_character_)
  m
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML('
      .qwrap {text-align:center;margin:12px 0 18px;overflow:auto;max-height:66vh;}
      .qimg {max-width:100%;height:auto;display:block;margin:auto;cursor:zoom-in;}
      .qimg.enlarged {max-width:none;max-height:none;}
      .actionbar {display:flex;flex-wrap:wrap;gap:8px;margin:12px 0;}
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
    document.addEventListener('click',function(e){
      if(e.target.classList.contains('qimg'))e.target.classList.toggle('enlarged');
    });"))
  ),
  titlePanel('정보시스템감리사 기출 CBT (2015~2025)'),
  p('정답 표시가 제거된 문제지 PDF를 선택하면 파일명에서 연도를 인식하고, 고정 answer_key.csv로 채점합니다.'),
  sidebarLayout(
    sidebarPanel(
      selectInput('pdf_file', '① 정답 제거 PDF 선택',
                  choices=c('PDF를 선택하세요'='', stats::setNames(get_clean_pdfs(), get_clean_pdfs())),
                  selected=''),
      textOutput('chosen_year'),
      actionButton('build', 'CBT 문제 불러오기', class='btn-primary'),
      hr(), textOutput('build_status'), hr(),
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
  current <- reactiveVal(1L)
  picked <- reactiveVal(integer(0))
  checked <- reactiveVal(logical(0))
  finished <- reactiveVal(FALSE)
  image_version <- reactiveVal(0L)
  status <- reactiveVal('정답 제거 PDF를 선택한 뒤 문제를 불러오세요.')
  output$build_status <- renderText(status())

  output$chosen_year <- renderText({
    req(input$pdf_file)
    year <- get_pdf_year(input$pdf_file)
    if (is.na(year)) return('파일명에서 시험 연도를 확인할 수 없습니다.')
    paste0('자동 인식 연도: ', year, '년 (A형 정답)')
  })

  observeEvent(input$build, {
    req(input$pdf_file)
    selected_name <- basename(input$pdf_file)
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
    status('PDF에서 문제 이미지를 만드는 중입니다...')
    tryCatch({
      pos <- withProgress(message='문항 이미지 생성', value=0, {
        create_question_images(selected_pdf, target_dir,
          progress=function(value, detail) setProgress(value=value, detail=detail))
      })
      hit <- match(pos$number, db$number)
      pos$answer <- db$answer[hit]
      pos$status <- db$status[hit]
      pos$status[is.na(pos$status)] <- 'UNKNOWN'
      pos$answer_keys <- vapply(seq_len(nrow(pos)), function(i) key_for_row(pos$answer[i], pos$status[i]), character(1))
      pos$image_url <- paste0('auto_questions/', year, '_', typ, '/', pos$image)
      pos$year <- year; pos$type <- typ
      questions(pos)
      picked(rep(NA_integer_, nrow(pos)))
      checked(rep(FALSE, nrow(pos)))
      current(1L)
      finished(FALSE)
      image_version(image_version() + 1L)
      missing_q <- setdiff(db$number, pos$number)
      status(sprintf('%s년 | %s | 이미지 %d개 / 정답 확인 %d개 / 미확정 %d개%s',
        year, basename(selected_pdf), nrow(pos), sum(!is.na(pos$answer_keys)), sum(is.na(pos$answer_keys)),
        if (length(missing_q)) paste0(' | 미추출 문제 ', paste(missing_q, collapse=',')) else ''))
      showNotification('문제 불러오기 완료', type='message')
    }, error=function(e) {
      status(paste('오류:', conditionMessage(e)))
      showNotification(conditionMessage(e), type='error', duration=NULL)
    })
  })

  output$jump_ui <- renderUI({
    q <- questions(); if (is.null(q)) return(NULL)
    selectInput('jump', '문제 번호 이동', choices=stats::setNames(seq_len(nrow(q)), paste0(q$number,'번')))
  })
  observeEvent(input$jump, {
    q <- questions(); if (is.null(q)) return()
    n <- suppressWarnings(as.integer(input$jump))
    if (!is.na(n) && n >= 1L && n <= nrow(q)) current(n)
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
  move <- function(delta) { q <- questions(); if (is.null(q)) return(); current(max(1L, min(nrow(q), current() + delta))) }
  observeEvent(input$prev_btn, move(-1L))
  observeEvent(input$next_btn, move(1L))
  observeEvent(input$random_btn, { q <- questions(); if (!is.null(q)) current(sample.int(nrow(q), 1)) })
  observeEvent(input$finish_btn, finished(TRUE))
  observeEvent(input$resume_btn, finished(FALSE))
  observeEvent(input$clear_btn, {
    q <- questions(); if (is.null(q)) return()
    picked(rep(NA_integer_, nrow(q))); checked(rep(FALSE, nrow(q)))
    finished(FALSE); current(1L)
  })

  output$main_ui <- renderUI({
    q <- questions()
    if (is.null(q)) {
      return(tagList(h4('정답 제거 PDF를 선택해 CBT를 시작하세요.'),
                     p('정답 PDF는 필요하지 않습니다. answer_key.csv의 고정 정답으로 채점합니다.')))
    }
    if (finished()) return(tagList(h3('최종 결과'), uiOutput('stats'), actionButton('resume_btn', '문제로 돌아가기')))
    n <- current()
    tagList(
      h3(sprintf('%s년 · %d번 (%d / %d)', q$year[n], q$number[n], n, nrow(q))),
      div(class='qwrap', tags$img(class='qimg', alt=paste0(q$number[n], '번 문제'), src=paste0(q$image_url[n], '?v=', image_version()))),
      p(class='text-muted', '이미지 클릭: 확대/축소'),
      radioButtons('selected', '답안 선택', choices=stats::setNames(as.character(1:4), circle_labels),
                   selected=if (is.na(picked()[n])) character(0) else as.character(picked()[n]), inline=TRUE),
      div(class='actionbar', actionButton('prev_btn', '이전 (P)'),
          actionButton('check_btn', '정답 확인 (C)', class='btn-primary'),
          actionButton('next_btn', '다음 (N)'), actionButton('finish_btn', '결과 보기', class='btn-success')),
      uiOutput('feedback')
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
    a <- picked(); valid <- !is.na(a) & !is.na(q$answer_keys)
    correct <- sum(is_correct_answer(a, q$answer_keys) & valid)
    paste0('응답 ', sum(!is.na(a)), ' / ', nrow(q), '\n정답 ', correct, ' / 채점 ', sum(valid),
           '\n정답 미확정 ', sum(is.na(q$answer_keys)))
  })

  output$stats <- renderUI({
    q <- questions(); req(q)
    a <- picked(); valid <- !is.na(a) & !is.na(q$answer_keys)
    ok <- is_correct_answer(a, q$answer_keys)
    right <- valid & ok; wrong <- valid & !ok
    tagList(
      h4(sprintf('정답률: %s', if (sum(valid)) sprintf('%.1f%%', 100 * sum(right) / sum(valid)) else '채점 대상 없음')),
      p(sprintf('응답 %d / %d · 정답 %d · 오답 %d · 미확정 문항 %d', sum(!is.na(a)), nrow(q), sum(right), sum(wrong), sum(is.na(q$answer_keys)))),
      p('오답 문제: ', if (any(wrong)) paste(q$number[wrong], collapse=', ') else '없음')
    )
  })

  output$save_csv <- downloadHandler(
    filename=function() {
      q <- questions()
      if (is.null(q)) 'cbt_result.csv' else paste0('cbt_', q$year[1], '_', Sys.Date(), '.csv')
    },
    content=function(file) {
      q <- questions(); req(q)
      a <- picked(); valid <- !is.na(a) & !is.na(q$answer_keys)
      ok <- is_correct_answer(a, q$answer_keys)
      write.csv(data.frame(year=q$year, type=q$type, number=q$number,
                           selected=a, answer=q$answer_keys, correct=ifelse(valid, ok, NA), status=q$status),
                file, row.names=FALSE, fileEncoding='UTF-8')
    }
  )
}

shinyApp(ui=ui, server=server)
