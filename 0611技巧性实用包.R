install.packages("naniar", destdir ="D://Application//RStudio//packages")
library("janitor")
library(dplyr)
#clean_names可以帮助清理变量名
fake_raw <- tibble::tribble(
  ~id, ~`count/num`, ~W.t, ~Case, ~`time--d`, ~`%percent`,
  1L, "china", 3L, "w", 5L, 25L,
  2L, "us", 4L, "f", 6L, 34L,
  3L, "india", 5L, "q", 8L, 78L
)
fake_raw
fake_raw %>%
janitor::clean_names()
#count可以帮助计数，tabyl可以计算列百分比
data(mtcars)
mtcars%>%
  count(cyl)
mtcars%>%
  janitor::tabyl(cyl)

df <- tribble(
  ~id, ~date, ~store_id, ~sales,
  1, "2020-03-01", 1, 100,
  2, "2020-03-01", 2, 100,
  3, "2020-03-01", 3, 150,
  4, "2020-03-02", 1, 110,
  5, "2020-03-02", 3, 101
)
#get_dupes()按照指定列排序并把这一列提到最前面
df %>%
  janitor::get_dupes(store_id)
df %>%
  arrange(date)
df %>%
  janitor::get_dupes(date)

#自动生成公式：equatiomatic
library(equatiomatic)
mod1<-lm(mpg~cyl+disp,mtcars)
extract_eq(mod1)
extract_eq(mod1,use_coefs = TRUE) #看是不是需要加上系数

#自动进行模型评估
library(performance)
library(see)
library(ggrepel)
library(qqplotr)
performance::check_model(mod1)

#自动生成summary表格
library(gtsummary)
gtsummary::trial %>%
  dplyr::select(trt,age,grade,response)%>%
  gtsummary::tbl_summary(by=trt,missing="no")%>%
  gtsummary::add_p()%>%
  gtsummary::add_overall()%>%
  gtsummary::add_n()%>%
  gtsummary::bold_labels()

#回归的表格和生存函数的表格合并在一起
t1 <- glm(response ~ trt + age + grade, gtsummary::trial, family = binomial) %>%
  gtsummary::tbl_regression(exponentiate = TRUE)
t2 <- survival::coxph(survival::Surv(ttdeath, death) ~ trt + grade + age, gtsummary::trial) %>%
  gtsummary::tbl_regression(exponentiate = TRUE)
gtsummary::tbl_merge(
  tbls = list(t1, t2),
  tab_spanner = c("**Tumor Response**", "**Time to Death**")
)

#配色
devtools::install_github('cttobin/ggthemr')
library(ggthemr)
mtcars%>%
  mutate(cyl=factor(cyl))%>%
  ggplot(aes(x=mpg,fill=cyl,color=cyl))+
           geom_density(alpha=0.75)+
           labs(fill="Cylinders",color="cylinders",x="MPG",y="Density")+
           legend_bottom ()

#+ggthemr('pale', layout = 'scientific', spacing = 2, type = 'inner')

ggthemr_reset()
#多张图摆放
library(patchwork)
p1 <- ggplot(mtcars) +
  geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) +
  geom_boxplot(aes(gear, disp, group = gear))
p3 <- ggplot(mtcars) +
  geom_smooth(aes(disp, qsec))
p1 + p2 + p3

#缺失值处理
library(naniar)
airquality %>%
  group_by(Month) %>%
  naniar::miss_var_summary()
