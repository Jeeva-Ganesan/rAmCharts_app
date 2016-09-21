shinyServer(function(input, output) {
  
  output$uiOptions <- renderUI({
    list_res <- list()
    i <- 1
    
    # chart title
    list_res[[i]] <- textInput("title", "Title", value = "My chart title")
    i <- i + 1
    
    if(input$sidebar == "column") {
      if(input$col_type == 1) {
        if(is.null(input$input_data)) {
          data(data_bar)
        } else {
          data_bar <- read.table(input$input_data$path, sep = ";", dec = ".", header = TRUE)
        }
      } else if(input$col_type %in% c(2, 3, 4)) {
        if(is.null(input$input_data)) {
          data(data_gbar)
          data_bar <- data_gbar
        } else {
          data_bar <- read.table(input$input_data$path, sep = ";", dec = ".", header = TRUE)
        }
      } else {
        if(is.null(input$input_data)) {
          data(data_fbar)
          data_bar <- data_fbar
        } else {
          data_bar <- read.table(input$input_data$path, sep = ";", dec = ".", header = TRUE)
        }
      }
      
      # numeric columns
      col_num <- colnames(data_bar)[which(sapply(colnames(data_bar), FUN = function(j) {
        is.numeric(data_bar[,j])
      }))]
      
      # categorical or character column
      col_char <- colnames(data_bar)[which(sapply(colnames(data_bar), FUN = function(j) {
        is.character(data_bar[,j]) | is.factor(data_bar[,j])
      }))]
      
      # x column
      list_res[[i]] <- radioButtons("col1_xcol", "X-axis value", choices = col_char)
      i <- i + 1
      
      if(input$col_type == 1) {
        # y column
        list_res[[i]] <- radioButtons("col1_ycol", "Y-axis value", choices = col_num)
        i <- i + 1
      } else if(input$col_type %in% c(2, 3, 4)){
        # y column
        list_res[[i]] <- checkboxGroupInput("col2_ycol", "Y-axis value", choices = col_num, selected = col_num)
        i <- i + 1
      } else {
        list_res[[i]] <- radioButtons("col5_ycol1", "Y-axis inf. value", choices = col_num)
        i <- i + 1
        list_res[[i]] <- radioButtons("col5_ycol2", "Y-axis sup. value", choices = col_num)
        i <- i + 1
      }
      
      #xlab 
      list_res[[i]] <- textInput("col1_xlab", "X-axis label", value = "X-axis label") 
      i <- i + 1
      
      #ylab
      list_res[[i]] <- textInput("col1_ylab", "Y-axis label", value = "Y-axis label")
      i <- i + 1
      
      # colors
      # if("")
      
      # horizontal?
      list_res[[i]] <- checkboxInput("col1_horiz", "Horizontal chart")
      i <- i + 1
      
      # show values ?
      list_res[[i]] <- checkboxInput("col1_showval", "Show values")
      i <- i + 1
      
      # 3D ?
      list_res[[i]] <- checkboxInput("col1_3d", "3D")
      i <- i + 1
      
      # legend ?
      list_res[[i]] <- checkboxInput("legend1", "Legend")
      i <- i + 1
      
      # if(input$legend1) {
      list_res[[i]] <- conditionalPanel(
        condition = "input.legend1", 
        radioButtons("legend_side1", "Legend side", choices = c("left", "right", "top", "bottom")))
      i <- i + 1
      # }
    } else if(input$sidebar == "line") {
      
      if(is.null(input$input_data)) {
        data("iris")
        data_lig <- iris
      } else {
        data_lig <- read.table(input$input_data$path, sep = ";", dec = ".", header = TRUE)
      }
      
      #colonnes numeriques
      col_num <- colnames(data_lig)[which(sapply(colnames(data_lig), FUN = function(j) {
        is.numeric(data_lig[,j])
      }))]
      
      # colonne x
      list_res[[i]] <- radioButtons("lig_xcol", "X-axis value", choices = col_num)
      i <- i + 1
      
      # colonne y
      list_res[[i]] <- checkboxGroupInput("lig_ycol", "Y-axis value", choices = col_num)
      i <- i + 1
      
      #xlab 
      list_res[[i]] <- textInput("lig_xlab", "X-axis label", value = "X-axis label") 
      i <- i + 1
      
      #ylab
      list_res[[i]] <- textInput("lig_ylab", "Y-axis label", value = "Y-axis label")
      i <- i + 1
      
      
      # legend ?
      list_res[[i]] <- checkboxInput("legend2", "Legend")
      i <- i + 1
      
      list_res[[i]] <- conditionalPanel(
        condition = "input.legend2", 
        radioButtons("legend_side2", "Legend side", choices = c("left", "right", "top", "bottom")))
      i <- i + 1
      
    } else if(input$sidebar == "piedon") {
      if(is.null(input$input_data)) {
        data("data_pie")
      } else {
        data_pie <- read.table(input$input_data$path, sep = ";", dec = ".", header = TRUE)
      }
      
      #colonnes numeriques
      col_num <- colnames(data_pie)[which(sapply(colnames(data_pie), FUN = function(j) {
        is.numeric(data_pie[,j])
      }))]
      
      #colonnes char
      col_char <- colnames(data_pie)[which(sapply(colnames(data_pie), FUN = function(j) {
        is.character(data_pie[,j])
      }))]
      
      list_res[[i]] <- radioButtons("pie_lab", "Labels column", choices = col_char)
      i <- i + 1
      
      list_res[[i]] <- radioButtons("pie_val", "Values column", choices = col_num)
      i <- i + 1
      
      # show values ?
      list_res[[i]] <- checkboxInput("pie_showval", "Show values")
      i <- i + 1
      
      # 3D ?
      list_res[[i]] <- sliderInput("pie_depth", "Pie depth", min = 0, max = 100, value = 0)
      i <- i + 1
      
      if(input$piedon_type == 2) {
        list_res[[i]] <- sliderInput("pie_radius", "Inner Radius", min = 0, max = 100, value = 0)
        i <- i + 1
      }
      
      # legend ?
      list_res[[i]] <- checkboxInput("legend3", "Legend")
      i <- i + 1
      
      list_res[[i]] <- conditionalPanel(
        condition = "input.legend3", 
        radioButtons("legend_side3", "Legend side", choices = c("left", "right", "top", "bottom")))
      i <- i + 1
    }
    
    return(list_res)
  })
  
  
  output$amchart <- renderAmCharts({
    if(input$sidebar == "column") {
      if(input$col_type == 1) {
        if(is.null(input$input_data)) {
          data(data_bar)
        } else {
          data_bar <- read.table(input$input_data$path, sep = ";", dec = ".", header = TRUE)
        }
        res <- amBarplot(x = input$col1_xcol, y = input$col1_ycol, data = data_bar, xlab = input$col1_xlab,
                         ylab = input$col1_ylab, horiz = input$col1_horiz, show_values = input$col1_showval,
                         depth = ifelse(input$col1_3d, 20, 0), main = input$title)
      } else if(input$col_type %in% c(2, 3, 4)) {
        if(is.null(input$input_data)) {
          data(data_gbar)
        } else {
          data_gbar <- read.table(input$input_data$path, sep = ";", dec = ".", header = TRUE)
        }
        
        if (input$col_type == 2) {
          res <- amBarplot(x = input$col1_xcol, y = input$col2_ycol, data = data_gbar, xlab = input$col1_xlab,
                           ylab = input$col1_ylab, horiz = input$col1_horiz, show_values = input$col1_showval,
                           depth = ifelse(input$col1_3d, 20, 0), main = input$title, legend = input$legend1,
                           legendPosition = ifelse(!is.null(input$legend_side1), input$legend_side1, "right")
          )
        } else if (input$col_type == 3) {
          res <- amBarplot(x = input$col1_xcol, y = input$col2_ycol, data = data_gbar, xlab = input$col1_xlab,
                           ylab = input$col1_ylab, horiz = input$col1_horiz, show_values = input$col1_showval,
                           depth = ifelse(input$col1_3d, 20, 0), main = input$title, stack_type = "regular", 
                           legend = input$legend1, 
                           legendPosition = ifelse(!is.null(input$legend_side1), input$legend_side1, "right")
          )
        } else if (input$col_type == 4) {
          res <- amBarplot(x = input$col1_xcol, y = input$col2_ycol, data = data_gbar, xlab = input$col1_xlab,
                           ylab = input$col1_ylab, horiz = input$col1_horiz, show_values = input$col1_showval,
                           depth = ifelse(input$col1_3d, 20, 0), main = input$title, stack_type = "100", 
                           legend = input$legend1, 
                           legendPosition = ifelse(!is.null(input$legend_side1), input$legend_side1, "right")
          )
        }
      } else if (input$col_type == 5) {
        if(is.null(input$input_data)) {
          data("data_fbar")
        } else {
          data_fbar <- read.table(input$input_data$path, sep = ";", dec = ".", header = TRUE)
        }
        res <- amFloatingBar(x = input$col1_xcol, y_inf = input$col5_ycol1, y_sup = input$col5_ycol2, data = data_fbar, 
                             xlab = input$col1_xlab, ylab = input$col1_ylab, horiz = input$col1_horiz, 
                             show_values = input$col1_showval, depth = ifelse(input$col1_3d, 20, 0), main = input$title, 
                             legendPosition = ifelse(!is.null(input$legend_side1), input$legend_side1, "right")
        )
      }
    } else if (input$sidebar == "line") {
      if(is.null(input$input_data)) {
        data("iris")
        data_lig <- iris
      } else {
        data_lig <- read.table(input$input_data$path, sep = ";", dec = ".", header = TRUE)
      }
      
      if(input$line_type == 1) {
        type_l <- "l"
      } else if(input$line_type == 2) {
        type_l <- "sl"
      } else if(input$line_type == 3) {
        type_l <- "st"
      } else if(input$line_type == 4) {
        type_l <- "b"
      }
      
      
      if(is.null(input$lig_ycol)) {
        yval <- NULL
      } else {
        yval <- data_lig[,input$lig_ycol]
      }
      
      if(!is.null(yval)) {
        data_lig <- data_lig[order(data_lig[,input$lig_xcol]),]
        if(length(input$lig_ycol) == 1) {
          res <- amPlot(x = data_lig[,input$lig_xcol], y = data_lig[,input$lig_ycol], xlab = input$lig_xlab,
                        ylab = input$lig_ylab, type = type_l, main = input$title, legend = input$legend2,
                        legendPosition = ifelse(!is.null(input$legend_side2), input$legend_side2, "right"))
        } else if(length(input$lig_ycol) > 1) {
          ycol1 <- input$lig_ycol[1]
          res <- amPlot(x = data_lig[,input$lig_xcol], y = data_lig[,ycol1], xlab = input$lig_xlab,
                        ylab = input$lig_ylab, type = type_l, main = input$title, legend = input$legend2,
                        legendPosition = ifelse(!is.null(input$legend_side2), input$legend_side2, "right"))
          sapply(2:length(input$lig_ycol), FUN = function(j) {
            ycoli <- input$lig_ycol[j]
            res <<- amLines(res, y = data_lig[,ycoli])
          })
        }
        
      } else {
        res <- amPlot(x = data_lig[,input$lig_xcol], xlab = input$lig_xlab,
                      ylab = input$lig_ylab, type = type_l, main = input$title, legend = input$legend2,
                      legendPosition = ifelse(!is.null(input$legend_side2), input$legend_side2, "right")
                      )
      }
    
    } else if(input$sidebar == "piedon") {
      
      if(is.null(input$input_data)) {
        data("data_pie")
      } else {
        data_pie <- read.table(input$input_data$path, sep = ";", dec = ".", header = TRUE)
      }
      
      colnames(data_pie)[which(colnames(data_pie) == input$pie_lab)] <- "label"
      colnames(data_pie)[which(colnames(data_pie) == input$pie_val)] <- "value"
      
      if(input$piedon_type == 1) {
        res <- amPie(data = data_pie, show_values = input$pie_showval, depth = input$pie_depth, main = input$title,
                     legend = input$legend3, 
                     legendPosition = ifelse(!is.null(input$legend_side3), input$legend_side3, "right"))
      } else {
        res <- amPie(data = data_pie, show_values = input$pie_showval, depth = input$pie_depth,
                     inner_radius = input$pie_radius, main = input$title, legend = input$legend3,  
                     legendPosition = ifelse(!is.null(input$legend_side3), input$legend_side3, "right"))
      }
    }
    
    
    res
  })
  
  output$table_chart <- renderDataTable({
    if(input$sidebar == "column") {
      if(input$col_type == 1) {
        if(is.null(input$input_data)) {
          data(data_bar)
        } else {
          data_bar <- read.table(input$input_data$path, sep = ";", dec = ".", header = TRUE)
        }
        res <- datatable(data_bar, rownames = FALSE)
      } else if(input$col_type %in% c(2, 3, 4)){
        if(is.null(input$input_data)) {
          data(gdata_bar)
        } else {
          data_gbar <- read.table(input$input_data$path, sep = ";", dec = ".", header = TRUE)
        }
        res <- datatable(data_gbar, rownames = FALSE)
      } else if(input$col_type == 5) {
        if(is.null(input$input_data)) {
          data(data_fbar)
        } else {
          data_fbar <- read.table(input$input_data$path, sep = ";", dec = ".", header = TRUE)
        }
        res <- datatable(data_fbar, rownames = FALSE)
      }
    } else if(input$sidebar == "line") {
      if(is.null(input$input_data)) {
        data(iris)
        res <- iris
      } else {
        res <- read.table(input$input_data$path, sep = ";", dec = ".", header = TRUE)
      }
    } else if(input$sidebar == "piedon") {
      if(is.null(input$input_data)) {
        data("data_pie")
        res <- data_pie
      } else {
        res <- read.table(input$input_data$path, sep = ";", dec = ".", header = TRUE)
      }
    }
    res
  })
  
  output$code_chart <- renderText({
    if(input$sidebar == "column") {
      if(input$col_type == 1) {
        res_text <- paste0('amBarplot(x = "', input$col1_xcol, '", y = "', input$col1_ycol, '", data = data_bar, xlab = "',
                           input$col1_xlab, '", ylab = "', input$col1_ylab, '", horiz = ', input$col1_horiz, 
                           ', show_values = ', input$col1_showval, ', depth = ', ifelse(input$col1_3d, 20, 0), 
                           ', main = "', input$title, '")')
      } else if(input$col_type == 2) {
        res_text <- paste0('amBarplot(x = "', input$col1_xcol, '", y = "', input$col1_ycol, '", data = data_bar, xlab = "',
                           input$col1_xlab, '", ylab = "', input$col1_ylab, '", horiz = ', input$col1_horiz, 
                           ', show_values = ', input$col1_showval, ', depth = ', ifelse(input$col1_3d, 20, 0), 
                           ', main = "', input$title, '", legend = ', input$legend1, ')')
      } else if(input$col_type == 3) {
        res_text <- paste0('amBarplot(x = "', input$col1_xcol, '", y = "', input$col1_ycol, '", data = data_bar, xlab = "',
                           input$col1_xlab, '", ylab = "', input$col1_ylab, '", horiz = ', input$col1_horiz, 
                           ', show_values = ', input$col1_showval, ', depth = ', ifelse(input$col1_3d, 20, 0), 
                           ', main = "', input$title, '", stacktype = "regular", legend = ', input$legend1, ')')
      } else if(input$col_type == 4) {
        res_text <- paste0('amBarplot(x = "', input$col1_xcol, '", y = "', input$col1_ycol, '", data = data_bar, xlab = "',
                           input$col1_xlab, '", ylab = "', input$col1_ylab, '", horiz = ', input$col1_horiz, 
                           ', show_values = ', input$col1_showval, ', depth = ', ifelse(input$col1_3d, 20, 0), 
                           ', main = "', input$title, '", stacktype = "100", legend = ', input$legend1, ')')
      } else if(input$col_type == 5) {
        res_text <- paste0('amFloatingBar(x = "', input$col1_xcol, '", y_inf = "', input$col5_ycol1, ", y_sup = ",
                           input$col5_ycol2, '", data = data_bar, xlab = "',
                           input$col1_xlab, '", ylab = "', input$col1_ylab, '", horiz = ', input$col1_horiz, 
                           ', show_values = ', input$col1_showval, ', depth = ', ifelse(input$col1_3d, 20, 0), 
                           ', main = "', input$title, '", stacktype = "100", legend = ', input$legend1, ')')
      }
    } else if (input$sidebar == "line") {
      if(is.null(input$input_data)) {
        data("iris")
        data_lig <- iris
      } else {
        data_lig <- read.table(input$input_data$path, sep = ";", dec = ".", header = TRUE)
      }
      
      if(input$line_type == 1) {
        type_l <- "l"
      } else if(input$line_type == 2) {
        type_l <- "sl"
      } else if(input$line_type == 3) {
        type_l <- "st"
      } else if(input$line_type == 4) {
        type_l <- "b"
      }
      
      
      if(is.null(input$lig_ycol)) {
        yval <- NULL
      } else {
        yval <- data_lig[,input$lig_ycol]
      }
      
      if(!is.null(yval)) {
        data_lig <- data_lig[order(data_lig[,input$lig_xcol]),]
        if(length(input$lig_ycol) == 1) {
          res_text <- paste0('amPlot(x = datalig$', input$lig_xcol,', y = data_lig$', input$lig_ycol, ', xlab = "', 
                             input$lig_xlab, '", ylab = "', input$lig_ylab, '", type = "', type_l, '", main = "', input$title, 
                             '", legend = ', input$legend2, ', legendPosition = "', 
                             ifelse(!is.null(input$legend_side2), input$legend_side2, 'right'), '")')
        } else if(length(input$lig_ycol) > 1) {
          ycol1 <- input$lig_ycol[1]
          res_text <- paste0('amPlot(x = data_lig$', input$lig_xcol, ', y = data_lig$', ycol1, ', xlab = "', input$lig_xlab,
                        '", ylab = "', input$lig_ylab, '", type = "', type_l, '", main = "', input$title, 
                        '", legend = ', input$legend2, ', legendPosition = "', 
                        ifelse(!is.null(input$legend_side2), input$legend_side2, 'right'), '") ')
          sapply(2:length(input$lig_ycol), FUN = function(j) {
            ycoli <- input$lig_ycol[j]
            res_text <<- paste0(res_text, 
                                '%>>% amLines(y = data_lig$', ycoli, ') ')
          })
        }
        
      } else {
        res_text <- paste0('amPlot(x = data_lig$', input$lig_xcol, ', xlab = "', input$lig_xlab,
                      '", ylab = "', input$lig_ylab, '", type = "', type_l, '", main = "', input$title, '", legend = ', input$legend2,
                      ', legendPosition = "', ifelse(!is.null(input$legend_side2), input$legend_side2, 'right'), '")')
        
      }
    } else if(input$sidebar == "piedon") {
      
      if(is.null(input$input_data)) {
        data("data_pie")
      } else {
        data_pie <- read.table(input$input_data$path, sep = ";", dec = ".", header = TRUE)
      }
      
      res_text <- paste0('colnames(data_pie)[which(colnames(data_pie) == "', input$pie_lab, '")] <- "label"', '\n',
                         'colnames(data_pie)[which(colnames(data_pie) == "', input$pie_val, '", )] <- "value"', '\n')
      
      
      if(input$piedon_type == 1) {
        res_text <- paste0(res_text,
                          'amPie(data = data_pie, show_values = ', input$pie_showval, ', depth = ', input$pie_depth, 
                          ', main = "', input$title, '", legend = ', input$legend3, 
                     ', legendPosition = "', ifelse(!is.null(input$legend_side3), input$legend_side3, "right"), '")')
      } else {
        res_text <- paste0(res_text,
                           'amPie(data = data_pie, show_values = ', input$pie_showval, ', depth = ', input$pie_depth, 
                           ', inner_radius = ', input$pie_radius, 
                           ', main = "', input$title, '", legend = ', input$legend3, 
                           ', legendPosition = "', ifelse(!is.null(input$legend_side3), input$legend_side3, "right"), '")')
      }
    }
    
    
    HTML(paste0("<pre><code>", res_text, "</code></pre>"))
  })
  
  
})
