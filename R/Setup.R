library(tidyverse)

data.dir = "/Users/ysl/Desktop/UIUC/STAT/STAT385/DataScientist-analysis/data/data_cleaned_2021.csv"
salary = read_csv(data.dir)

salary = salary |> 
  select(-c("Job Title", "Salary Estimate", "Job Description",
            "Founded", "Type of ownership",
            "Competitors", "Hourly", "Employer provided",
            "Age", "Industry", "Headquarters", "Company Name",
            "Location", "Lower Salary", "Upper Salary",
            job_title_sim, seniority_by_title, Degree)) |>
  filter(Size != "unknown") |>
  filter(Revenue != "Unknown / Non-Applicable") |>
  rename(avgsalary = "Avg Salary(K)", location = "Job Location")

write_csv(x = salary, file = "data/salary.csv")
