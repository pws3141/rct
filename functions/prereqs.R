colours <- c(
  "Waiting" = "#66CCEE",
  "Removed" = "#DDCC77",
  "Died" = "#BBBBBB",
  "Transplanted" = "#228833"
)
colours_trans <- alpha(colours, alpha = 0.5)

# factors considered text:
factors_not_included <- HTML(
  "<strong>Recipient BMI</strong> – Tested and not found to be significant in the model.<br><br>
  
  <strong>Creatinine</strong> – The kidney function of donor is available but not
  included. This is because it's not possible to know how many were on titration in
  ITU. This would give a falsely low creatinine and potentially be misleading.<br><br>
  
  <strong>Comorbidities (cardiovascular disease, vascular disease, stroke,
  MI)</strong> – Not collected, looked into those that are, have a high proportion of
  missing data.<br><br>
  
  <strong>Time on dialysis</strong> – Recipient waiting time (years). Time waiting on
  deceased donor kidney waiting list until time of transplant (active and suspended).
  This can serve as a proxy for 'time on dialysis' as most patients are either already
  on dialysis or due to commence dialysis within 6 months at the time of listing for
  transplantation."
)

# --------------------------------------------------
# ------------ Home cards
homeCards <- list(
  card(card_header("Kidney Transplants: Understanding the Numbers"),
       #
       h4(tags$b("Who is this tool for?")),
       p("This tool is for people over 18 who are considering kidney transplantation."),
       p("It should be used with a tranplant doctor, specialist nurse or other healthcare professional."),
       h4(tags$b("What does this tool do?")),
       p("This is a communication tool. It will help clinicians explain numbers about transplants to patients."),
       p("It will show graphs and charts of waiting times and survival outcomes that are more personalised to each patient."),
       p("Results of the tool can be printed for patients to take home."),
       h4(tags$b("What will it show?")),
       tags$ul(
         tags$li("Likely waiting time"),
         tags$li("How long the kidney might last"),
         tags$li("How long the patient might survive after a transplant")
       ),
       h4(tags$b("How does it work?")),
       p("The tool takes information about the patient and calculates what happened to people in the registry who had the same characteristics."),
       #p("Even more words, with a ",
       #  tags$a(href = "https://digital.nhs.uk/data-and-information/statistical-publications-open-data-and-data-products",
       #                        "link"), 
       #  " to the webpage with the report on."),
       p(sprintf("This dashboard was last updated on %s.", Sys.Date())),
       br()
  ),
  card(card_header("What will it show?"),
       tagList(
         tags$ul(
           tags$li("The tool uses data from the NHSBT registry. It can only use data that is collected by NHSBT. In some cases data is not yet collected or has missing values to the extent that they cannot be included in the tool. If you want to know more about the data and models behind the tool, see the Technical section."),
           tags$li("It cannot yet take into account everything about the patient e.g. other health conditions or about the donor e.g. DCD or DBD donors."),
           tags$li(
             tagList(
               "The waiting times tool was created to be accurate at ",
               tags$b("time of listing."),
               "It is not accurate if it is used while the patient is on the waiting list."
             )
           ),
           tags$li(
             tagList(
               "The survival tool was created to be accurate at ",
               tags$b("time of transplant."),
               "It is not accurate if it is used after transplantation."
             )
           ),
           tags$li("There are many factors that influence how well a transplant does and many of those cannot be factored into the tool (yet).")
         )
       )
  )
)

aboutCards <- list(
  card(card_header("Model development"),
       h4("Overview"),
       p("The tool takes information about you, such as age, blood group, disease, and it looks at people who had these same characteristics, and shows what happened to these people. For example, how many people 'like you' received a transplant within one year of being listed."),
       p("It is not showing you what will happen to you, it is showing you what happened to people like you, in the past."),
       p("It’s important to remember that the tool cannot take into account everything about you, for example, whether you have other health conditions. The tool will ask for some medical information such as blood group, or recent test results. The tool will be less accurate if you don't have all the relevant information."),
       p("There are many factors that can influence how well a transplanted organ does, for example taking your medication properly, diet and whether you exercise."),
       p("If you want to know more about the models and data behind the tools, please read the Technical section. Data about transplant patients were used to create statistical models. When you enter information into the tool, the calculator looks at these models and produces results."),
       p("⚠ Changes to the UK Kidney Offering Scheme in September 2019 are not reflected in these models"),
       h4("Using the tool offline"),
       p("You need an internet connection to access the tool for the first time, but once you have visited the site once, you can access it offline (just don't close the browser)."),
       p("The tool can produce a printout of results for later reference."),
       h4("Who is this site for?"),
       p("The tool is suitable for kidney patients who are over 18 years old. This is because we use past data from the NHS transplant registry. Fewer children have transplants than adults and there is not enough data yet to make a tool for children."),
       p("The tool should be used by patients alongside their transplant doctors or coordinator."),
       h4("Who developed the tool?"),
       p("The tool was developed by NHS Blood and Transplant, with help from the Winton Centre for Risk and Evidence Communication."),
       p("We wish to thank all the transplant patients and their partners, as well as clinical teams at transplant centres in England who took part in researching the tool design.")
  )
)

publicationCard <- list(
  card(card_header("Publications"),
       p("NHS Blood and Transplant Organ Speicific Annual Report: ",
         tags$a(href = "https://www.odt.nhs.uk/statistics-and-reports/organ-specific-reports/",
                               "https://www.odt.nhs.uk/statistics-and-reports/organ-specific-reports/"))
  )
)
  
techPanels <- list(
  nav_panel(title = "Model development",
            p("The models behind the tool were developed using UK Transplant Registry (UKTR) data which is held by NHS Blood and Transplant (NHSBT). The UKTR database contains information on all patients who are listed for transplantation in the UK, and all patients who are transplanted with a solid organ transplant in the UK with follow-up data."),
            p("NHSBT Statisticians work closely with transplant clinicians to compile a large list of potential variables (e.g. age, primary renal disease) from the UK Transplant Registry to test in their models. Each of these variables are statistically tested and kept in the model if found to have an important relationship with the outcome of interest (e.g. post-transplant survival). These variables are referred to as ‘risk factors’. Some of the models used by the tool are also used regularly by NHSBT in their organ specific annual reports (",
              tags$a(href = "https://www.odt.nhs.uk/statistics-and-reports/organ-specific-reports/", "https://www.odt.nhs.uk/statistics-and-reports/organ-specific-reports/"),
              ") and in other analyses."),
            p("At the end of the modelling process values were obtained called ‘parameter estimates’ which quantify the estimated impact of each risk factor upon the outcome of interest. Please refer to the Mathematical Section below to see exactly how a change in parameter estimates affects the outcome of interest. There will also be an estimated baseline risk curve plotted over time that represents an ‘average’ patient in the study cohort. The most common/mean value from the model development dataset for each risk factor is indicated as the baseline value as this value is represented by the baseline curve. The parameter estimates are then used by the tool to essentially shift this baseline curve when the values of the risk factors are changed from the ‘average’ values. This way, the patient can plot a curve for values of the risk factors that are relevant to their own circumstances. For all models, transplant centre was treated as a stratifying factor, i.e. a separate baseline curve was produced for each centre."),
            p("Although the tool is based on reputable models, it cannot say what the outcomes for a particular patient will be. It can only provide a summary of survival and waiting list outcomes for people in the past with similar characteristics."),
            p("This tool has been developed using retrospective registry data. Therefore, changes to the Kidney Offering Scheme in 2019 are NOT reflected in these models."),
            p("All statistical analyses for this website were generated using SAS Enterprise Guide software, Version 7.1. SAS and all other SAS Institute Inc. product or service names are registered trademarks or trademarks of SAS Institute Inc., Cary, NC, USA.")
  ),
  nav_panel(title = "Waiting Times",
            p("The dataset used for this model comprised of all adult (aged ≥18 years) first kidney-only registrations (i.e. people joining the transplant waiting list) between 1 January 2010 and 31 December 2015."),
            p("From the point of joining the waiting list, receiving a transplant is one of three competing events (transplant, death on the list, removal from the list) that a patient is 'at risk' of. We considered outcome data up to 5 years from listing for all patients in the modelling cohort. A model for 'time to transplant', a model for 'time to death on the list' and a model for 'time to removal from the list' was then developed using Cox Regression (Section 3.1)."),
            p("Each patient in the cohort was assigned to 1 of 4 categories:"),
            tags$ul(
              tags$li("Patients who were transplanted with either a living or deceased donor transplant"),
              tags$li("Patients who died on the list whilst awaiting transplantation"),
              tags$li("Patients who were removed from the list prior to transplantation. This could occur for a number of reasons including patient choice or a deterioration in health such that a transplant was no longer suitable."),
              tags$li("Patients who were still waiting on the list. Patients who were suspended were classed as still waiting on the list.")
            ),
            p("The covariates used in the model were those which have previously been shown to have an impact on outcome and those which were thought to be clinically significant."),
            p("Following development of the Cox Proportional hazards models, a numerical approximation algorithm was applied which combined the model results from the time to transplant model with the time to death on the list/removal from the list model. This algorithm enabled the estimated chances of each of the listed outcomes at any particular time up to three years post-listing to be presented side-by-side and the sum of the probabilities of each of these happening at a particular time t to equal 100%.")
  ),
  nav_panel(title = "Input Factors", 
            "Explanation of donor and recipient input covariates:",
            tags$ul(
              tags$li("Recipient age (years) - Age at point of being actively listed onto the National Kidney Transplant List. This has been divided into categories by decade."),
              tags$li("Sex – Male or female. Note this refers to sex, not gender."),
              tags$li("Recipient ethnicity – Asian, Black, Chinese, Mixed, White, Other."),
              tags$li("Recipient waiting time (years) – Time waiting on deceased donor kidney waiting list until time of transplant (active and suspended). This can serve as a proxy for ‘time on dialysis’ as most patients are either already on dialysis or due to commence dialysis within 6 months at the time of listing for transplantation."),
              tags$li("Previous Kidney Transplant? – Yes or No."),
              tags$li("Highly sensitised (cRF >85%) – Any antibodies in the blood – e.g. as a result of pregnancy or a previous organ transplant."),
              tags$li("Blood group – Patient’s blood group: O, A, B, AB."),
              tags$li("Dialysis at registration – Refers to any form of dialysis (peritoneal or haemodialysis) at the time of listing for transplantation."),
              tags$li(
                tagList(
                  "Matchability – Whether due to a range of factors, such as blood group, it will be ‘easy’, ‘moderate’, or ‘difficult’ to find a matching organ. The ODT provides further details on how this is calculated and a tool for calculating matchability for individual patients:",
                  tags$a(href = "https://www.odt.nhs.uk/transplantation/tools-policies-and-guidance/calculators/", 
                         "https://www.odt.nhs.uk/transplantation/tools-policies-and-guidance/calculators/")
                )
              ),
              tags$li("Donor age – The age at which the donor donated their organs."),
              tags$li("Donor BMI – Donor BMI as recorded at the donating hospital site. Calculated as weight (kilograms) divided by height (m²)."),
              tags$li("Donor Hypertension – Whether the donor suffered from high blood pressure as recorded by NHSBT on data collection forms at the time of donation."),
              tags$li("HLA MM level – Human Leukocyte Antigen (HLA) matching level. HLA are proteins located on the surface of white blood cells and other tissues. When people share the same HLA’s, they are said to be a ‘match’. There are many different types of HLA, and the matching can occur to different degrees, hence the different levels of matching."),
              tags$li("Transplant Centre – This refers to which of the 23 UK adult kidney transplant centres the patient will be receiving their transplant. (This is not always the dialysis centre at which they are followed up).")
            )
  ),
  nav_panel(title = "Patient and graft survival after deceased donor transplant",
            p("The patient cohort for these models comprised all adult (aged ≥18 years) first kidney-only transplants that occurred in the UK between 1 January 2010 and 31 December 2017. Cox proportional hazards models were built where the following 22 factors were tested for inclusion in the models: Donor age, type, cause of death, sex, cmv status, hypertension, BMI, height, weight retrieval creatinine, recipient age, ethnicity, sex diabetic nephropathy as a cause of renal failure, waiting time, matchability, blood group, cold ischaemia time and HLA mismatch. Factors tested were those collected by NHSBT and available on the database and thought to potentially be clinically relevant. The model was built using a forward-step approach. Transplant centre was added to the model as a strata."),
            p("The post-transplant survival Cox proportional hazards model operates such that each risk factor multiplies the baseline cumulative hazard by a fixed amount known as the hazard ratio or relative risk - essentially the proportional change in mortality risk. This means the cumulative hazard is the product of two components: the baseline hazard (chances of death or graft failure for a patient with a baseline set of characteristics at time of transplant) and the hazard ratios for the risk factors (the increased/decreased risk of death due to changes in these risk factors compared to the baseline characteristics). The cumulative hazard is then translated in to a survival function as described in the mathematical description."),
            tags$h4("Five-year deceased donor post-transplant patient survival"),
            p("Post-transplant patient survival was defined as the time from transplant until the time of death. These data were censored at the last known follow-up date post-transplant if this was within 5 years of transplantation. The following factors were found to be significant and included in the model; recipient age, recipient ethnicity, waiting time, recipient primary renal disease, donor age, donor hypertension, HLA MM level."),
            p("This model was tested for goodness of fit using a concordance statistic (c-statistic) which was found to be 0.71."),
            tags$h4("Five-year deceased donor post-transplant graft survival"),
            p("'Graft survival' refers to death-censored graft survival and was defined as the time from transplantation to return to long-term kidney replacement therapy or re-transplantation, whichever occurred first. Data were censored at the time of death or at last known follow-up. The following factors were found to be significant and included in the model; recipient age, waiting time, graft number, recipient primary renal disease, donor age, donor BMI, donor hypertension, HLA MM level."),
            p("This model was tested for goodness of fit using a concordance statistic (c-statistic) which was found to be 0.63.")
  ),
  
  nav_panel(title = "Patient and graft survival after living donor transplant",
            p("The patient cohort for these models comprised all adult (aged ≥18 years) first kidney-only transplants that occurred in the UK between 1 January 2010 and 31 December 2015."),
            p("Cox proportional hazards models were built where the following 17 factors were tested for inclusion in the models -"),
            p(tags$strong("Donor factors:"), " age, sex, relationship to recipient, BMI, ethnicity, status, hypertension, BMI, height, weight retrieval creatinine."),
            p(tags$strong("Recipient factors:"), " age, sex, ethnicity, diabetic nephropathy as a cause of renal failure, waiting time, dialysis status, matchability, blood group, cold ischaemia time, HLA mismatch and graft number."),
            p("Factors tested were those collected by NHSBT and available on the database and thought to potentially be clinically relevant. The model was built using a forward-step approach. Due to fewer numbers, the transplant centre was not included as a strata and national results have been demonstrated."),
            tags$h4("Five-year living donor post-transplant graft survival"),
            p("'Graft survival' refers to death-censored graft survival and was defined as the time from transplantation to return to long-term kidney replacement therapy or re-transplantation, whichever occurred first. Data were censored at the time of death or at last known follow-up."),
            p("The following factors were found to be significant and included in the model; recipient age, waiting time, matchability, donor age, HLA MM level.")
  ),
  nav_panel(title = "Mathematical Section", 
            tagList(
              tags$p("A Cox proportional hazards model was adopted. This multiplies a baseline cumulative hazard by a constant hazard ratio for each risk factor."),
              tags$p("Using the example of post-transplant survival, this means that the cumulative hazard of post-transplant death is the product of two components: the baseline hazard (chances of dying for a patient with a baseline set of characteristics at time of transplant) and all the hazard ratios for the risk factors (the increased/decreased risk of death due to changes in these risk factors compared to the baseline characteristics)."),
              tags$p("In the case of post-transplant survival, the cumulative hazard is then translated into a survival function. This is described in mathematical form below."),
              tags$p("The estimated cumulative hazard for the \\(i^\\text{ th}\\) individual for the event of interest (e.g. death post-transplant), after \\(t\\) days has the form:"),
              tags$p("$$H_i(t) = H_0(t) \\exp(\\beta \\cdot \\chi_i)$$"),
              tags$p("where:"),
              tags$ul(
                tags$li("\\(H_0(t)\\) is estimated using the Breslow (1972) estimate."),
                tags$li("The log hazard ratios \\(\\beta\\) are estimated by a multivariate linear regression."),
                tags$li("\\(\\chi_i\\) represents the set of characteristics for the \\(i^\\text{ th}\\) individual.")
              ),
              tags$p("This can be translated into a survival function through the following equation:"),
              tags$p("$$S_i(t) = \\exp(-H_i(t))$$"),
              tags$p("In the case of the ‘Waiting time’ models, we apply an iterative algorithm to calculate the risks of all the competing outcomes."),
              tags$p("The ", tags$code("phreg"), " function in SAS V.7.1 (SAS Institute, Cary, North Carolina, USA) was used to compute these estimates.")
            ),
  nav_panel(title = "The web implementation",
            p("This tool is a Single Page Application - an SPA. It is a single web page which loads a Javascript application that updates the page according to the user's inputs. All data that you enter to the tool is stored in Javascript variables in the browser."),
            p("The application is also a calculator. The Javascript code includes implementations of all the Cox statistical models described above. This means that all inputs, calculations, and result displays are managed without the need for any interaction with another machine. The model calculations run once you have entered all necessary data, and will rerun whenever you change any input. Once you close the browser window or tab, the data is erased, just like in a calculator."),
            p("The tool is also a Progressive Web App - a PWA - which means that in some ways it behaves like an application you might have downloaded onto your phone from an App Store. Once you load the app from the website, it is automatically cached in your browser for future use - offline if need be. You can also install the app so it appears as an icon on your home page. You should be able to find some relevant instructions by searching the internet for 'Install PWA' with your browser's name (e.g. Edge, Safari, Chrome, Firefox), and your operating system (e.g. IOS, Android, MacOS, Windows, Linux)."),
            tags$h4("The development stack"),
            p("The tool runs as a Javascript application, but it was written in Clojurescript and then compiled to Javascript. The most important libraries that it uses are ReactJS, Reagent, and Reframe, and we are sincerely grateful to the developers of these codes. The development system used Shadow-cljs (by Thomas Heller), supported by a number of Clojure scripts running under Babashka (by Michiel Borkent) and the Clojure integrated development system Calva running in VSCode."),
            tags$h4("Testing"),
            p("The reference for our implementation was a collection of canonical R implementations of the statistical models. We generated a large collection of test inputs and ran these through the R code, collected the results, and then fed the same inputs into our Javascript implementation, and compared the results."),
            tags$h4("Browser Compatibility"),
            p("This version has been tested and found to work in Edge, Chrome, Safari, Firefox, on desktop PCs and Macs and also on Android and IOS mobile devices."),
            p("Support for IE 11 is limited and some functionalities like 'Copy' or 'Fullscreen' may not work at all."),
            p("It does not currently support any other version of Internet Explorer.")
  )
            
  )
)

link_nhsbt <- tags$a(bsicons::bs_icon("house"), "NHS Blood and Transplant website", href = "https://www.nhsbt.nhs.uk/", target = "_blank")
link_otdt <- tags$a(bsicons::bs_icon("clipboard2-pulse"), "OTDT Website", href = "https://www.odt.nhs.uk/", target = "_blank")

transplantCentres <- c(
  "Belfast City Hospital",
  "Birmingham Queen Elizabeth Hospital",
  "Bristol Southmead Hospital",
  "Cambridge Addenbrooke's Hospital",
  "Cardiff University Hospital of Wales",
  "Coventry University Hospital",
  "Edinburgh Royal Infirmary",
  "Glasgow Western Infirmary",
  "Leeds St James's University Hospital",
  "Leicester General Hospital",
  "Liverpool Royal University Hospital",
  "London: Guy's Hospital",
  "London: St George's Hospital",
  "London: The Royal Free Hospital",
  "London: The Royal London Hospital",
  "London: West London Renal and Transplant Centre"
)