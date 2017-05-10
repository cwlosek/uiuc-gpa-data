library(shiny)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  observe({

    # Update Options for Select Input

    updateSelectInput(session, "Course",
                      choices = c(" ", unique(fs1516$Course[fs1516$Subject == input$Subject])),
                      selected = input$Course)


    if(nrow(fs1516[fs1516$Subject == input$Subject & fs1516$Course == input$Course, ]) == 1){
      updateSelectInput(session, "Title",
                        choices = unique(fs1516$`Course Title`[fs1516$Subject == input$Subject & fs1516$Course == input$Course]))
    }
    else{
      updateSelectInput(session, "Title",
                        choices = unique(fs1516$`Course Title`[fs1516$Subject == input$Subject & fs1516$Course == input$Course]),
                        selected = input$Title)
    }

    updateSelectInput(session, "Professor",
                      choices = c("", fs1516_prof$FirstLast[fs1516_prof$Subject == input$Subject & fs1516_prof$Course == input$Course
                                                            & fs1516_prof$`Course Title` == input$Title]),
                      selected = input$Professor)

    if(input$Theme){
      theme1 = list("rgb(251,129,29)", "rgb(0,60,124)", "rgb(251,129,29)", "rgb(0,60,124)",
                    "rgb(251,129,29)", "rgb(0,60,124)", "rgb(251,129,29)", "rgb(0,60,124)",
                    "rgb(251,129,29)", "rgb(0,60,124)", "rgb(251,129,29)", "rgb(0,60,124)", "rgb(251,129,29)")

      theme2 = list("rgb(0,60,124)", "rgb(251,129,29)", "rgb(0,60,124)", "rgb(251,129,29)",
                    "rgb(0,60,124)", "rgb(251,129,29)", "rgb(0,60,124)", "rgb(251,129,29)",
                    "rgb(0,60,124)", "rgb(251,129,29)", "rgb(0,60,124)", "rgb(251,129,29)", "rgb(0,60,124)")
    }
    else{
      theme1 = "rgb(158,202,225)"

      theme2 = "rgb(8,48,107)"
    }

    # Plot - Barplot (individual course) & Interactive Scatterplot (all courses)

    if(input$display == "Grade Distributions" & input$Subject != "" & input$Course != "" & input$Title != ""){
      output$gpaPlot = renderPlotly({


        plot_ly(y = ~as.numeric(fs1516[fs1516$Subject == input$Subject &
                                         fs1516$Course == input$Course &
                                         fs1516$`Course Title` == input$Title, ][16:4]),
                x = ~barplotGradeNames,
                type = "bar",
                marker = list(color = theme1,
                              line = list(color = theme2,
                                          width = 1.5))) %>%
          layout(title = paste("Grade Distribution:", input$Subject, input$Course, "-", input$Title),
                 xaxis = list(title = "Letter Grade", categoryarray = ~barplotGradeNames, categoryorder = "array"),
                 yaxis = list(title = "Number of Students"))
      })
    }
    else{
      output$gpaPlot = renderPlotly({
        return()
      })
    }

    output$scatterplot = renderPlotly({
      if(input$display == "Scatter Plot (All Courses)"){
        plot_ly(x = ~PerctFour, y = ~GPA, data = fs1516) %>%
          layout(title = "Scatterplot of All Courses", xaxis = list(title = "Percentage of 4.00\'s"),
                 yaxis = list(title = "Average GPA")) %>%
          add_markers(x = ~PerctFour, y = ~GPA, color = ~GPA,
                      hoverinfo = "text",
                      text = ~paste(fs1516$Subject, paste0(fs1516$Course, ":"), fs1516$`Course Title`,
                                    "<br / > GPA:", fs1516$GPA,
                                    "<br / > 4.00's (A+/A) Given:", paste0(fs1516$PerctFour, "%")))
      }
      else{
        return()
      }

    })


    # Text - Information about selected course, shown with barplot

    if(input$display == "Grade Distributions" & input$Subject != "" & input$Course != "" & input$Title != ""){
      output$text1 = renderText({

        paste("Overall Course GPA:",
              fs1516$GPA[fs1516$Subject == input$Subject &
                           fs1516$Course == input$Course &
                           fs1516$`Course Title` == input$Title],
              "|",
              "Percentage of 4.00\'s Given:",
              paste0(fs1516$PerctFour[fs1516$Subject == input$Subject &
                                        fs1516$Course == input$Course &
                                        fs1516$`Course Title` == input$Title], "%"))
        # "|",
        # "Credit Hours: Currently Unavailable")
      })

      output$text2 = renderText({

        paste(c("Recent Professor(s):", paste(fs1516_prof$FirstLast[fs1516_prof$Subject == input$Subject &
                                                                      fs1516_prof$Course == input$Course &
                                                                      fs1516_prof$`Course Title` == input$Title], collapse = ", ")))

      })
    }
    else{
      output$text = renderText("")
      output$text2 = renderText("")
    }

    if(input$display == "Grade Distributions" &
       input$Subject != "" & input$Course != "" &
       input$Title != "" & input$Professor != ""){

      output$text3 = renderText({

        paste("Individual GPA for", paste0(input$Professor, ":"), fs1516_prof$GPA[fs1516_prof$Subject == input$Subject &
                                                                                    fs1516_prof$Course == input$Course &
                                                                                    fs1516_prof$`Course Title` == input$Title &
                                                                                    fs1516_prof$FirstLast == input$Professor])
      })

    }
    else{
      output$text3 = renderText("")
    }

    # Table - Gen ed table

    if(input$display == "Search by Gen Ed"){
      output$table = renderDataTable({
        if(is.null(input$geCategory)){
          genedsTable
        }
        else{

          genedsIndicies2 = reactive({
            genedsIndicies = rep(NA, 7)

            if("Advanced Composition" %in% input$geCategory){
              genedsIndicies[1] = "No"
            }
            else{
              genedsIndicies[1] = ""
            }

            if("Non-Western Culture" %in% input$geCategory){
              genedsIndicies[2] = "No"
            }
            else{
              genedsIndicies[2] = ""
            }

            if("Western/Comparative Culture" %in% input$geCategory){
              genedsIndicies[3] = "No"
            }
            else{
              genedsIndicies[3] = ""
            }

            if("Humanities & the Arts" %in% input$geCategory){
              genedsIndicies[4] = "No"
            }
            else{
              genedsIndicies[4] = ""
            }

            if("Natural Science & Technology" %in% input$geCategory){
              genedsIndicies[5] = "No"
            }
            else{
              genedsIndicies[5] = ""
            }

            if("Quantitative Reasoning" %in% input$geCategory){
              genedsIndicies[6] = "No"
            }
            else{
              genedsIndicies[6] = ""
            }

            if("Social & Behavioral Sciences" %in% input$geCategory){
              genedsIndicies[7] = "No"
            }
            else{
              genedsIndicies[7] = ""
            }
            return(genedsIndicies)
          })

          genedsTable[genedsTable$`Advanced Composition` != genedsIndicies2()[1] &
                        genedsTable$`Non-Western Culture` != genedsIndicies2()[2] &
                        genedsTable$`Western-Comparative Culture` != genedsIndicies2()[3] &
                        genedsTable$`Humanities & the Arts` != genedsIndicies2()[4] &
                        genedsTable$`Natural Science & Technology` != genedsIndicies2()[5] &
                        genedsTable$`Quantitative Reasoning` != genedsIndicies2()[6] &
                        genedsTable$`Social & Behavioral Sciences` != genedsIndicies2()[7], ]
        }
      })
    }
  })
})
