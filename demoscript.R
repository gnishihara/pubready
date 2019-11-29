# Making publication ready fonts
# 2019 Nov 29
# Greg Nishihara
#
# I use this method because the formatting looks better than using
# the showtext package. This is because of the way plotmat works.
# By using this method, I am using the methods from latex, which makes
# really good-looking documents and the fonts just look good.
# However, it is slower and more complicated.
######################################################################
# This will only work on a Debian 10 system with an installation
# of latex (tex live) and the following fonts:
#   English fonts: Noto Serif, Noto Sans
#   Source code fonts: Source Code Pro
#   Japanese fonts: Noto Serif CJK JP, Noto Sans CJK JP
# Packages needed for this script ------------------------------------
library(tidyverse)  # Data manipulation, includes ggplot2
library(ggpubr)     # Some utilities to make publication ready plots
library(lemon)      # More utilities to make publication ready plots
library(tikzDevice) # The tikz device to create latex plots
library(tinytex)    # Utilities to maintain a tex live installation
library(magick)     # Utilities to call imagemagick from R
library(tsibble)    # Utilities to work with ts objects in the tidyverse
# Sometimes the locale for time might be setup wrong.
# To ensure that any dates in the plot are not in Japanese,
# Check the LC_TIME environment variable -----------------------------
if(Sys.getlocale("LC_TIME") == "ja_JP.UTF-8") {
  Sys.setlocale("LC_TIME", "en_US.UTF-8") # Change to American English
}
# These options are for the tikz package to ensure that the fonts
# will work in the plot.
# To change the font, take a look at the following
#   \\setmainfont[]{}
#   \\setsansfont[]{}
#   \\setCJKmainfont[]{}
# Change the value in the {} to switch fonts. Right now I choose the
# main font to be Noto Sans.
options(tikzXelatexPackages = c(
  "\\usepackage{tikz}\n",
  "\\usepackage[active,tightpage,xetex]{preview}\n",
  "\\usepackage{mathspec,xunicode}\n",
  "\\PreviewEnvironment{pgfpicture}\n",
  "\\setlength\\PreviewBorder{0pt}\n",
  "\\defaultfontfeatures{Ligatures=TeX,Scale=MatchLowercase}\n",
  "\\setmainfont[]{Noto Sans}\n",
  "\\setsansfont[]{Noto Sans}\n",
  "\\setmonofont[Mapping=tex-ansi]{Source Code Pro}\n",
  "\\usepackage{xeCJK}\n",
  "\\setCJKmainfont[]{Noto Sans CJK JP}\n",
  "\\usepackage{microtype}\n",
  "\\UseMicrotypeSet[protrusion]{basicmath}\n",
  "\\usepackage{textgreek}\n"
), tikzDefaultEngine="xetex")

# Making the plot ----------------------------------------------------
# I will use the iris dataset to demo the script. I prefer tibbles, so
# I will convert iris to a tibble.
irist = iris %>% as_tibble()
# Prepare the axis labels. There is nothing special here. Since there are
# no special formatting codes, these labels will look fine on-screen.
xlabel = "Sepal length (cm)"
ylabel = "Petal length (cm)"
# These are the labels for the legend. Note the \\textit{} syntax, which
# is a latex syntax with an extra \. You need the extra \ to "escape"
# the slash, otherwise you will get an error. \\textit{} will make the
# text italic. Notice how it is different from the expression() method.
clabel = c("setosa"     = "\\textit{Iris setosa}",
           "versicolor" = "\\textit{Iris versicolor}",
           "virginica"  = "\\textit{Iris virginica}")
# Make the ggplot here.
# theme_pubr() is a theme from the ggpubr package. Note that no functions
# from the lemon package are being used here.
pout = ggplot(irist) +
  geom_point(aes(x = Sepal.Length, y = Petal.Length, color = Species))  +
  scale_x_continuous(xlabel) +
  scale_y_continuous(ylabel) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Dark2"),
                     labels = clabel) +
  theme_pubr() +
  theme(strip.background=element_blank(),
        strip.text=element_blank(),
        legend.position=c(0.0,1),
        legend.justification=c(0.0,1),
        legend.title=element_blank(),
        legend.background=element_blank())
# Take a look at it on screen. ---------------------------------------
# Note the legend and also note that the
# selected fonts are not applied. They will only be applied when the
# plot is saved.
pout
# Setup the parameter to create the plot on file. --------------------
wh = 80/25.4 # Convert 80 mm to inches
ht = 80/25.4 # Convert 80 mm to inches
pt = 11      # Font pointsize
filename = "mytikzplot.tex" # filename of the latex file.

# This is the code to open the tikz device, write out the plot to the device,
# and then to close the device.
# IMPORTANT: Always close the device with dev.off(), otherwise subsequent
# plots will be written to the same device - the will be written to the same file.
# Depending on the number of elements in the plot, this can take a while.
tikz(filename, width = wh, height = ht, pointsize=pt, standAlone=TRUE)
print(pout) # Needs to be in print() otherwise, it will not run when sourced.
dev.off() # Always close the tikz() device with dev.off()!!!
# Finally, the latex file needs to be compiled with xetex (xelatex). This
# can be done through the tinytex package.
# This will produce a PDF of the plot.
xelatex(filename)
# If you want the plot to be png, then you need to convert it.
DPI = 1200 # Resolution in dpi.
pdfname = str_replace(filename, pattern="tex", replacement="pdf")
pngname = str_replace(pdfname, pattern="pdf", replacement="png")
fout = image_read_pdf(pdfname, density = DPI)
image_write(fout, path = pngname)
# To save it as a tif it is a bit more complicated.
# It has to be done directly on the command line (terminal) outside of R.
# But, you can also do it through a system() call.
# Note that I used lzw compression on the tif, otherwise the file will be huge.
tifname = str_replace(pdfname, pattern="pdf", replacement="tif")
run = str_glue("convert -density {DPI} {pdfname} -compress lzw {tifname}")
system(run)

# Second example -----------------------------------------------------
# Another example with more complex formatting
# Here I am using the co2 data in R.
# Since it is a ts class, I am converting it to a tsibble by calling
# as_tsibble() from the tsibble package
co2t = co2 %>% as_tsibble() %>%
  mutate(value = 1000*value/44.0095)

xlabel = "Time (years)"
ylabel = "CO\\textsubscript{2} concentration (\\textmu mol L\\textsuperscript{-1})"
pout = ggplot(co2t) +
  geom_line(aes(x = index, y = value)) +
  scale_x_date(xlabel) +
  scale_y_continuous(ylabel) +
  theme_pubr() +
  theme(strip.background=element_blank(),
        strip.text=element_blank(),
        legend.position=c(0.0,1),
        legend.justification=c(0.0,1),
        legend.title=element_blank(),
        legend.background=element_blank())
wh = 80/25.4 # Convert 80 mm to inches
ht = 80/25.4 # Convert 80 mm to inches
pt = 11      # Font pointsize
filename = "mytikzplot2.tex" # filename of the latex file.
tikz(filename, width = wh, height = ht, pointsize=pt, standAlone=TRUE)
print(pout)
dev.off()
xelatex(filename)
DPI = 1200 # Resolution in dpi.
pdfname = str_replace(filename, pattern="tex", replacement="pdf")
pngname = str_replace(pdfname, pattern="pdf", replacement="png")
fout = image_read_pdf(pdfname, density = DPI)
image_write(fout, path = pngname)
tifname = str_replace(pdfname, pattern="pdf", replacement="tif")
run = str_glue("convert -density {DPI} {pdfname} -compress lzw {tifname}")
system(run)

# Third example ------------------------------------------------------
# The original ylabel is too long so I shortened it with \\smal{}
idm = Indometh %>% as_tibble()
idmlabel = idm %>% expand(Subject) %>% mutate(label = LETTERS[1:n()])
xlabel = "Time (hours)"
# ylabel = "Indometacin plasma concentration (mcg ml\\textsuperscript{-1})"
ylabel = "\\small{Indometacin plasma concentration (mcg ml\\textsuperscript{-1})}"
pout = ggplot(idm, aes(x = time, y = conc)) +
  geom_point() +
  geom_text(aes(x = 8, y = 3, label = label),
            hjust = 1, vjust = 1,
            data = idmlabel) +
  scale_x_continuous(xlabel, limits = c(0, 8)) +
  scale_y_continuous(ylabel, limits = c(0, 3)) +
  facet_rep_wrap("Subject") +
  theme_pubr() +
  theme(strip.background=element_blank(),
        strip.text=element_blank(),
        legend.position=c(0.0,1),
        legend.justification=c(0.0,1),
        legend.title=element_blank(),
        legend.background=element_blank())
wh = 2*80/25.4 # Convert 160 mm to inches
ht = 80/25.4 # Convert 80 mm to inches
pt = 11      # Font pointsize
filename = "mytikzplot3.tex" # filename of the latex file.
tikz(filename, width = wh, height = ht, pointsize=pt, standAlone=TRUE)
print(pout)
dev.off()
xelatex(filename)
DPI = 1200 # Resolution in dpi.
pdfname = str_replace(filename, pattern="tex", replacement="pdf")
pngname = str_replace(pdfname, pattern="pdf", replacement="png")
fout = image_read_pdf(pdfname, density = DPI)
image_write(fout, path = pngname)
tifname = str_replace(pdfname, pattern="pdf", replacement="tif")
run = str_glue("convert -density {DPI} {pdfname} -compress lzw {tifname}")
system(run)
