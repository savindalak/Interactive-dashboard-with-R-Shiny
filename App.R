library(data.table)
library(XML)
library(curl)
library(dplyr)
library(ggplot2)
library(viridis) 
library(shinydashboard)
library(shiny)
library(ggpubr)
library(gridExtra)
library(shinyWidgets)
library(shinythemes)
library(fitdistrplus)
library(readxl)


#=====================Load file ===============================================================
file<-read_excel('Machine_data_Aitken.xls')
names(file)

file$Date<- as.Date(file$Date,format='%d/%m/%Y')
#====================Add columns===============================================================
file<-file%>%mutate(MTBF=No_of_hours_per_wk/No_of_breakdowns)%>%mutate(
  MTTR=Repair_time_per_wk/No_of_completed_repairs)%>%mutate(
    Failure_rate=No_of_breakdowns/No_of_hours_per_wk)%>%mutate(
      Safety_issues_per_machine_hr=Safety_issues/No_of_hours_per_wk)%>%mutate(
        cum_repair_cost=cumsum(Repair_cost))%>%mutate(
          cum_budget=cumsum(Budgeted_cost))%>%mutate(
            ave_no_hours_before_failure =No_of_hours_per_wk/No_of_breakdowns)%>%mutate(
              year=strftime(Date,'%Y'))%>%mutate(numeric_year=as.numeric(year))
names(file)
mid_num<-round(length(file$Date)/2,digits = 0)

#====================Failure rate graph========================================================
failure_rate_start<-mean(file$Failure_rate[1:mid_num])
failure_rate_end<-mean(file$Failure_rate[mid_num:length(file$Date)])

failure_rate_plot<-file%>%arrange(Date)%>%ggplot(aes(x=Date,y=Failure_rate))+geom_point()+theme(
  axis.text.x = element_text(  angle = 90, hjust =1,vjust=0))+geom_path()+ggtitle(
    'Failure Rate Monitoring Chart')+ylab('Failure Rate (Failures / Hr)')+geom_segment(
      aes(x=Date[1],xend=Date[mid_num],y=failure_rate_start,yend=failure_rate_start
      ),linetype="3313",col='red',size=1)+geom_segment(
        aes(x=Date[mid_num],xend=Date[length(Date)],y=failure_rate_end,yend=failure_rate_end
        ),linetype="3313",col='blue',size=1)+theme(plot.title =element_text(
          face = 'bold',size = 16),panel.background = element_rect(
            fill = "#BFD5E3",colour = "black",size =1, linetype = "solid"
          ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))
failure_rate_plot

#====================MTBF Graph================================================================

MTBF_start = mean(file$MTBF[1:mid_num])
MTBF_end=mean(file$MTBF[mid_num:length(file$Date)])

MTBF_plot<-file%>%arrange(Date)%>%ggplot(aes(x=Date,y=MTBF))+geom_point()+theme(
  axis.text.x = element_text(  angle = 90, hjust =1,vjust=0))+geom_path()+ggtitle(
    'MTBF Monitoring chart')+ylab('MTBF (Hours) ')+geom_segment(
      aes(x=Date[1],xend=Date[mid_num],y=MTBF_start,yend=MTBF_start),linetype="3313",col='red'
      ,size=1)+geom_segment(
        aes(x=Date[mid_num],xend=Date[length(Date)],y=MTBF_end,yend=MTBF_end
        ),linetype="3313",col='blue',size=1)+theme(plot.title =element_text(
          face = 'bold',size = 16),panel.background = element_rect(
            fill = "#BFD5E3",colour = "black",size =1, linetype = "solid"
          ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))
MTBF_plot

x=file$Date[1:mid_num]
x
#===========================MTTR Graph=========================================================

MTTR_plot<-file%>%arrange(Date)%>%ggplot(aes(x=Date,y=MTTR,fill=(MTTR>10)))+geom_col()+theme(
  axis.text.x = element_text(  angle = 90, hjust =1,vjust=0))+ggtitle(
    'Weekly MTTR Distribution')+ylab('MTTR in Hours')+geom_hline(
      aes(yintercept=mean(MTTR)),color="blue", linetype="dashed", size=1)+scale_fill_manual(
        values=c("#1CE015", "#F96C0A"))+theme(plot.title =element_text(
          face = 'bold',size = 16),panel.background = element_rect(
            colour = "black",size =1, linetype = "solid"
          ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))
MTTR_plot

#===========================Machine working hours graph=======================================

working_hours_plot<-file%>%arrange(Date)%>%ggplot(aes(x=Date,y=No_of_hours_per_wk,fill=(
  No_of_hours_per_wk<1200)))+geom_col()+theme(
    axis.text.x = element_text( angle = 90, hjust =1,vjust=0))+ggtitle(
      'Weekly Machine Working Hours')+ylab('Working Hours')+guides(fill=guide_legend(
        title='Working hours<1200'))+scale_fill_manual(values=c("blue", "red"))+theme(plot.title =element_text(
          face = 'bold',size = 16),panel.background = element_rect(
            fill = "#F6F6F0",colour = "black",size =1, linetype = "solid"
          ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))
working_hours_plot
#==========================Safety issues chart================================================

safety_issues_plot<-file%>%arrange(Date)%>%ggplot(aes(x=Date,y=Safety_issues_per_machine_hr))+geom_point(color='yellow',size=3)+theme(
  axis.text.x = element_text(  angle = 90, hjust =1,vjust=0))+geom_path(size=1)+ggtitle(
    'Safety Monitoring Chart')+ylab('Safety issues per machine hr')+theme(
      plot.title =element_text(face = 'bold',size = 16),panel.background = element_rect(
        fill = "#FC5A4F",colour = "black",size =1, linetype = "solid"
      ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))
safety_issues_plot

#==========================Employee Attendance Level==========================================

attendance_plot<-file%>%arrange(Date)%>%ggplot(aes(x=Date,y=Attendance_percentage))+geom_point()+theme(
  axis.text.x = element_text(  angle = 90, hjust =1,vjust=0))+ggtitle(
    'Weekly Employee Attendance Level')+ylab('% Attendance Level')+geom_path()+geom_hline(
      aes(yintercept=mean(Attendance_percentage)),color="blue", linetype="dashed", size=1
    )+theme(plot.title =element_text(
      face = 'bold',size = 16),panel.background = element_rect(
        fill = "#9DFC29",colour = "black",size =1, linetype = "solid"
      ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))
attendance_plot

#==========================Cost vs Budget plot==================================================

repair_cost<- file%>%ggplot(aes(x=Date))+geom_line(
  aes(y=cum_budget,col='Budgeted Cost'),size=1)+geom_line(aes(
    y=cum_repair_cost,col='Repair Cost'),size=1)+ylab('Cost') + ggtitle(
      'Cumulative actual cost vs Cumulative Budgeted cost')+theme(
        legend.title = element_blank())+scale_colour_manual(values=c("green","red"))+theme(
          plot.title =element_text(face = 'bold',size = 16),panel.background = element_rect(
            fill = "#ECECD3",colour = "black",size =1, linetype = "solid"
          ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))


repair_cost
#==========================Weibull Reliability plot========================================================

weibull_final<- function(input_year='NA'){

if (input_year=='NA'){
  
  filtered_data<-file
}
else{
  
  filtered_data<-file%>%filter(year==input_year)
}

wbl_param<-fitdist(filtered_data$ave_no_hours_before_failure,'weibull')
shape<-wbl_param$estimate['shape']
scale<-wbl_param$estimate['scale']

filtered_data$ave_no_hours_before_failure

tail<-tail(filtered_data$ave_no_hours_before_failure,n=1)
t<-(1:tail)

a<- exp(-(t/scale)^shape)

pdf<- (shape/scale)*(t/scale)^(shape-1)*exp(-(t/scale)^shape)

weibull_plot<-ggplot(mapping=aes(x=t,y=a))+geom_line(size=1)+ylab(
  'Density')+xlab('Hours')+ggtitle(paste('Weibull Reliability plot in'),input_year)+theme(plot.title =element_text(
    face = 'bold',size = 16),panel.background = element_rect(
      fill = "#BFD5E3",colour = "black",size =1, linetype = "solid"
    ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))

weibull_pdf<-ggplot(mapping=aes(x=t,y=pdf))+geom_line()

tab <- c(Shape_factor = round(shape,digits = 2),
         scale_factor = round(scale,digits = 2))
row_names<-c('Shape','Scale')

p_tab <- tableGrob(tab,rows = row_names)
grid.arrange(weibull_plot, p_tab, ncol =2,widths=c(4,1))


  p_tab <- tableGrob(tab,rows = row_names)
  grid.arrange(weibull_plot, p_tab, ncol =2,widths=c(4,1))
  
}




#=====================Cost per cycle graph====================================================

cost_per_cycle_plot<-file%>%mutate(cost_per_box= Repair_cost/No_of_boxes)%>%ggplot(
  aes(x=Date,y=cost_per_box,fill=(cost_per_box>100)))+geom_col()+geom_hline(
    aes(yintercept=mean(cost_per_box)),color="blue", linetype="dashed", size=1)+ylab(
      'Cost per cyple (LKR)')+scale_fill_manual(values=c("#28E5D2", "#E52847"),labels=c(
        'Under 100','Over 100'),name='')+theme(plot.title =element_text(
          face = 'bold',size = 16),panel.background = element_rect(
            colour = "black",size =1, linetype = "solid"),panel.grid.major = element_line(
              size = 0.5, linetype = 'solid',colour = "white"))+ggtitle('Repair cost per cycle')
cost_per_cycle_plot 

#========================Hypothesis testing===================================================


t_test_func<-function(input_year1,input_year2,variable){

X<-file%>%filter(year==input_year1)%>%dplyr::select(variable)
Y<-file%>%filter(year==input_year2)%>%dplyr::select(variable)

t_result<-t.test(x=X[[variable]], y =as.vector(Y[[variable]]), alternative = c("two.sided", "less", "greater"), mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)

t_result


}


#=========================Scatter plots=======================================================

A<-file%>%ggplot(aes(x=No_of_breakdowns,y=Safety_issues))+geom_point()+theme(
  plot.title =element_text(face = 'bold',size = 16),panel.background = element_rect(
    fill = "#BFD5E3",colour = "black",size =1, linetype = "solid"
    ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))+ggtitle(
      'No of Breakdowns vs Safety incidents')+ylab('No of Safety incidents')+xlab('No of breakdowns')

B<-file%>%ggplot(aes(x=No_of_hours_per_wk,y=No_of_boxes))+geom_point()+theme(
  plot.title =element_text(face = 'bold',size = 16),panel.background = element_rect(
    fill = "#ff4d88",colour = "black",size =1, linetype = "solid"
  ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))+ggtitle(
    'No of Working hours vs No of boxes handled')+ylab('No of Boxes')+xlab('No of Working hours')


C<-file%>%ggplot(aes(y=Repair_cost,x=No_of_breakdowns))+geom_point()+theme(
  plot.title =element_text(face = 'bold',size = 16),panel.background = element_rect(
    fill = "#99ffff",colour = "black",size =1, linetype = "solid"
  ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))+ggtitle(
    'Repair cost vs No of Breakdowns')+ylab('Repair Cost')+xlab('Breakdowns')


D<-file%>%ggplot(aes(x=No_of_hours_per_wk))+geom_histogram(fill='#602040')+theme(
  plot.title =element_text(face = 'bold',size = 16),panel.background = element_rect(
    fill = "#e6ffcc",colour = "black",size =1, linetype = "solid"
  ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))+ggtitle(
    'No of Working hours distribution')+ylab('Frequency')+xlab('Working hours')


E<-file%>%ggplot(aes(x=No_of_breakdowns))+geom_histogram(fill='#800000')+theme(
  plot.title =element_text(face = 'bold',size = 16),panel.background = element_rect(
    fill = "#ffcc66",colour = "black",size =1, linetype = "solid"
  ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))+ggtitle(
    'No of Breakdowns distribution')+ylab('Frequency')+xlab('Breakdowns')

F<-file%>%ggplot(aes(y=Repair_cost,x=MTTR))+geom_point()+theme(
  plot.title =element_text(face = 'bold',size = 16),panel.background = element_rect(
    fill = "#ffff4d",colour = "black",size =1, linetype = "solid"
  ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))+ggtitle(
    'Repair cost vs MTTR')+ylab('Repair Cost')+xlab('MTTR')


#================Render Dash board with shiny==================================================


ui<-dashboardPage(
  
  dashboardHeader(title = "RELIABILITY DASHBOARD",titleWidth = 1400
                  ,tags$li(a(href = 'https://dashboardforeverybusiness.business.site',img(
                    src = 'logo-maintenanceblog.jpg',title = "Company Home", width = "50px"
                  ),style = "padding-top:0px; padding-left:0px; padding-right:0px; padding-bottom:0px;"),class = "dropdown")),
  
  
  dashboardSidebar(
      
    
    
      sidebarMenu(
        
              menuItem(text = "All", icon = icon("fas fa-align-justify"), tabName = "all"),
              menuItem("Failure rate", icon = icon("fas fa-chart-line"), tabName = "failure_rate"),
              menuItem("Repair cost", icon = icon("fas fa-chart-line"), tabName = "repair_cost"),
              menuItem("MTTR", icon = icon("fas fa-chart-bar"), tabName = "MTTR"),
              menuItem("Working hours", icon = icon("fas fa-chart-bar"), tabName = "working_hours"),
              menuItem("Safety", icon = icon("fas fa-chart-line"), tabName = "safety"),
              menuItem("Attendance", icon = icon("fas fa-chart-line"), tabName = "attendance"),
              menuItem("Cost", icon = icon("fas fa-chart-bar"), tabName = "cost"),
              menuItem("MTBF", icon = icon("fas fa-chart-line"), tabName = "MTBF"),
              menuItem("Weibull plot", tabName = "weibull", icon = icon("fas fa-chart-line")),
              menuItem("Patterns", tabName = "patterns", icon = icon("fas fa-chart-bar")),
              menuItem(text = "Analysis", icon = icon("fas fa-align-justify"), tabName = "parts"))
      
      
      
      ),
               
               
               dashboardBody(
                 
                             tags$script(HTML("
                    var openTab = function(tabName){
                      $('a', $('.sidebar')).each(function() {
                        if(this.getAttribute('data-value') == tabName) {
                          this.click()
                        };
                      });
                    }
                  ")),
                             
                             tags$head( 
                               tags$style(HTML(".fa{font-size: 25px;}
                                   
                                 
                                  .skin-blue .main-header .logo{
                                  font-family: 'Georgia';
                                  font-weight: bold;
                                  font-size: 24px;
                                  width:1400px;
                                  color:#ffffffF;
                                  background-color:#000000;
                                  }
                                  
                                  /* active selected tab in the sidebarmenu */
                                  .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                  background-color: #ff0000;
                                  }
                                  
                                  /* logo when hovered */
                                  .skin-blue .main-header .logo:hover {
                                  background-color: #000000;
                                  }
                                  
                                  /* navbar (rest of the header) */
                                  .skin-blue .main-header .navbar {
                                  background-color: #000000;
                                  }
                                  
                                  /* other links in the sidebarmenu */
                                  .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                  background-color: #000000;
                                  
                                  }
                                  
                                  /* body */
                                  .content-wrapper, .right-side {
                                  background-color:  #a6ff4d;
                                  }
                                  
                                  /* main sidebar */
                                  .skin-blue .main-sidebar {
                                  background-color: #000000;
                                  }
                                 
                                   ")),
                               
                             ),
                 
                 
                 tabItems(
                   tabItem(tabName = "weibull",
                           h2("Weibull plot"),
                           div(sliderInput(inputId = 'year',label = 'Year', min = min(unique(file$numeric_year)),max = max(
                             unique(file$numeric_year)),value = min(unique(file$numeric_year))),style='font-weight:bold'),
                           plotOutput('plot10',height = 500),
                           tags$a(actionButton(label = ' Back ',inputId = 'back',style='padding:10px; font-weight:bold;font-size:20px;
                                               color:white;background-color:black'),onclick = "openTab('all')",href="#")
                           
                   ),
                   
                   tabItem(tabName = "failure_rate",
                           plotOutput('plot11',height = 600),
                           tags$a(actionButton(label = ' Back ',inputId = 'back',style='padding:10px; font-weight:bold;font-size:20px;
                                               color:white;background-color:black'),onclick = "openTab('all')",href="#")
                   ),
                   
                   tabItem(tabName = "repair_cost",
                           plotOutput('plot12',height = 600),
                           tags$a(actionButton(label = ' Back ',inputId = 'back',style='padding:10px; font-weight:bold;font-size:20px;
                                               color:white;background-color:black'),onclick = "openTab('all')",href="#")
                   ),
                   
                   tabItem(tabName = "MTTR",
                           plotOutput('plot13',height = 600),
                           tags$a(actionButton(label = ' Back ',inputId = 'back',style='padding:10px; font-weight:bold;font-size:20px;
                                               color:white;background-color:black'),onclick = "openTab('all')",href="#")
                   ),
                   
                   tabItem(tabName = "working_hours",
                           plotOutput('plot14',height = 600),
                           tags$a(actionButton(label = ' Back ',inputId = 'back',style='padding:10px; font-weight:bold;font-size:20px;
                                               color:white;background-color:black'),onclick = "openTab('all')",href="#")
                   ),
                   
                   tabItem(tabName = "safety",
                           plotOutput('plot15',height = 600),
                           tags$a(actionButton(label = ' Back ',inputId = 'back',style='padding:10px; font-weight:bold;font-size:20px;
                                               color:white;background-color:black'),onclick = "openTab('all')",href="#")
                   ),
                   
                   tabItem(tabName = "attendance",
                           plotOutput('plot16',height = 600),
                           tags$a(actionButton(label = ' Back ',inputId = 'back',style='padding:10px; font-weight:bold;font-size:20px;
                                               color:white;background-color:black'),onclick = "openTab('all')",href="#")
                   ),
                   
                   tabItem(tabName = "cost",
                           plotOutput('plot17',height = 600),
                           tags$a(actionButton(label = ' Back ',inputId = 'back',style='padding:10px; font-weight:bold;font-size:20px;
                                               color:white;background-color:black'),onclick = "openTab('all')",href="#")
                   ),
                   
                   tabItem(tabName = "MTBF",
                           plotOutput('plot18',height = 600),
                           tags$a(actionButton(label = ' Back ',inputId = 'back',style='padding:10px; font-weight:bold;font-size:20px;
                                               color:white;background-color:black'),onclick = "openTab('all')",href="#")
                   ),
                   
                   
                   
                   
                   
                   tabItem(
                            
                     tabName = "parts",
                     #div(h2("Parts requirement analysis"),style="color:black"),
                     column(5,div(("Parts requirement analysis"),style="font-size:25px;font-weight:bold"),numericInput(inputId = 'R_previous',value = 0, label = 'Previous Forecast'),
                           numericInput(inputId = 'X_previous',value = 0, label = 'Previous Actual'),
                           numericInput(inputId = 'alpha',value = 0, label = 'Smoothing Factor-enter value between 0.1-0.4'),
                           div(textOutput(outputId = 'R_current'),style='font-size:20px;font-weight:bold')),
                     
                     column(5,div(("Hypothesis Testing"),style="font-size:25px;font-weight:bold"), selectInput(inputId = 'year_1',choices = unique(file$year),label = 'Select year'),
                            selectInput(inputId = 'year_2',choices = unique(file$year), label = 'Select another year to compare'),
                            selectInput(inputId = 'variable_hypo',choices = c('Safety_issues','Failure_rate'), label = 'Select variable'),
                            div(textOutput(outputId = 'hypo_result'),style='font-size:20px;font-weight:bold'),
                            div(textOutput(outputId = 'text1'),style='font-size:20px;font-weight:bold'),
                            div(textOutput(outputId = 'text2'),style='font-size:20px;font-weight:bold'),
                            div(textOutput(outputId = 'p_value'),style='font-size:20px;font-weight:bold')
                            )
                     
                   ),
                   
                   tabItem(tabName = "all",
                           fluidRow(
                             
                            tags$a(column(4,plotOutput('plot1',height = 200),style='padding:10px'), onclick = "openTab('failure_rate')", href="#"),
                            tags$a(column(4,plotOutput('plot2',height = 200),style='padding:10px'),onclick = "openTab('repair_cost')", href="#"),
                            tags$a(column(4,plotOutput('plot3',height = 200),style='padding:10px'),onclick = "openTab('MTTR')", href="#"),
                            tags$a(column(4,plotOutput('plot4',height = 200),style='padding:10px'),onclick = "openTab('working_hours')", href="#"),
                            tags$a(column(4,plotOutput('plot5',height = 200),style='padding:10px'),onclick = "openTab('safety')", href="#"),
                            tags$a(column(4,plotOutput('plot6',height = 200),style='padding:10px'),onclick = "openTab('attendance')", href="#"),
                            tags$a(column(4,plotOutput('plot7',height = 200),style='padding:10px'),onclick = "openTab('cost')", href="#"),
                            tags$a(column(4,plotOutput('plot8',height = 200),style='padding:10px'),onclick = "openTab('MTBF')", href="#"),
                            tags$a(column(4,plotOutput('plot9',height = 200),style='padding:10px'),onclick = "openTab('weibull')", href="#"),
                           ),
                   ),
                   
                   tabItem(tabName = "patterns",
                           fluidRow(
                             
                             tags$a(column(4,plotOutput('A',height = 300),style='padding:10px')),
                             tags$a(column(4,plotOutput('B',height = 300),style='padding:10px')),
                             tags$a(column(4,plotOutput('C',height = 300),style='padding:10px')),
                             tags$a(column(4,plotOutput('D',height = 300),style='padding:10px')),
                             tags$a(column(4,plotOutput('E',height = 300),style='padding:10px')),
                             tags$a(column(4,plotOutput('F',height = 300),style='padding:10px')),
                             
                           ),
                   )
                   
                   
                   
                   
                   )
                 
                 
               )
)



server<- function(input,output){
  
  output$plot1<- renderPlot({
    
    failure_rate_plot
    
    
  })
  output$plot2<- renderPlot({
    
    repair_cost
    
  })
  
  output$plot3<- renderPlot({
    
    MTTR_plot
    
  })
  
  output$plot4<- renderPlot({
    
    working_hours_plot
    
  })
  
  output$plot5<- renderPlot({
    
    safety_issues_plot
    
  })
  
  output$plot6<- renderPlot({
    
    attendance_plot
    
  })
  
  output$plot7<- renderPlot({
    
    cost_per_cycle_plot
    
  })
  
  output$plot8<- renderPlot({
    
    MTBF_plot
    
  })
  
  output$plot9<- renderPlot({
    
    
    
   weibull_final()
    
  })
  
  output$plot10<- renderPlot({
    
    
  weibull_final(input$year)
    

  })
  
  output$plot11<- renderPlot({
    
    failure_rate_plot+theme(axis.text=element_text(size=16),
                            axis.title=element_text(size=18,face="bold"),
                            plot.title = element_text(size=22))
    
  })
  
  output$plot12<- renderPlot({
    
    repair_cost+theme(axis.text=element_text(size=16),
                      axis.title=element_text(size=18,face="bold"),
                      plot.title = element_text(size=22))
    
  })
  
  output$plot13<- renderPlot({
    
    MTTR_plot+theme(axis.text=element_text(size=16),
                      axis.title=element_text(size=18,face="bold"),
                      plot.title = element_text(size=22))
    
  })
  
  output$plot14<- renderPlot({
    
    working_hours_plot+theme(axis.text=element_text(size=16),
                    axis.title=element_text(size=18,face="bold"),
                    plot.title = element_text(size=22))
    
  })
  
  output$plot15<- renderPlot({
    
    safety_issues_plot+theme(axis.text=element_text(size=16),
                             axis.title=element_text(size=18,face="bold"),
                             plot.title = element_text(size=22))
    
  })
  
  output$plot16<- renderPlot({
    
    attendance_plot+theme(axis.text=element_text(size=16),
                             axis.title=element_text(size=18,face="bold"),
                             plot.title = element_text(size=22))
    
  })
  
  output$plot17<- renderPlot({
    
    cost_per_cycle_plot+theme(axis.text=element_text(size=16),
                          axis.title=element_text(size=18,face="bold"),
                          plot.title = element_text(size=22))
    
  })
  
  output$plot18<- renderPlot({
    
    MTBF_plot+theme(axis.text=element_text(size=16),
                              axis.title=element_text(size=18,face="bold"),
                              plot.title = element_text(size=22))
    
  })
  
  output$R_current <- renderText({
    
    
    result<-(input$X_previous)*(input$alpha) + (1-input$alpha)*(input$R_previous)
    #valueBox(value = result,subtitle = 'Parts requirement for next week',width = 3)
    print(paste('Parts requirement for next week is ',result))
    
  })
  
  output$hypo_result <- renderText({
    
    
    year1<- input$year_1
    year2<- input$year_2
    variable<- input$variable_hypo
    
    t_result<-t_test_func(year1,year2,variable)
    
    
    if (t_result[[3]]< 0.05){
      print(paste('There is a statistacally significant difference between two years '))
    }
    
    else{
      print(paste('There is NO statistacally significant difference between two years '))
     
          }
    
  })
  
  output$text1 <- renderText({
    
    
    year1<- input$year_1
    year2<- input$year_2
    variable<- input$variable_hypo
    
    t_result<-t_test_func(year1,year2,variable)
    print(paste('Mean of first selected year : ',round(t_result[[5]][[1]],digits =4)))
    
  })
  
  output$text2 <- renderText({
    
    
    year1<- input$year_1
    year2<- input$year_2
    variable<- input$variable_hypo
    
    t_result<-t_test_func(year1,year2,variable)
    print(paste('Mean of second selected year : ',round(t_result[[5]][[2]],digits =4)))
    
  })
  
  output$p_value <- renderText({
    
    
    year1<- input$year_1
    year2<- input$year_2
    variable<- input$variable_hypo
    
    t_result<-t_test_func(year1,year2,variable)
    print(paste('P-Value : ',t_result[[3]]))
    
  })
  
  output$A<- renderPlot({
    
    A
    
  })
  
  output$B<- renderPlot({
    
    B
    
  })
  
  output$C<- renderPlot({
    
    C
    
  })
  
  output$D<- renderPlot({
    
    D
    
  })
  
  output$E<- renderPlot({
    
    E
    
  })
  
  output$F<- renderPlot({
    
    F
    
  })

}

shinyApp(ui,server)



