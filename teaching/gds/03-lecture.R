library(sf)
library(tidyverse)

# remember to remove quiet=T if you have trouble!
weca = st_read('../data/weca.gpkg', quiet=T)
earnings = read_csv('../data/earnings.csv')
temps = read_csv('../data/bristol_temps.csv') %>% 
  pivot_longer(jan:dec, names_to='month', values_to='temperature') %>%
  mutate(month = ordered(month, levels=c("jan", "feb", "mar", "apr", 
                                         "may", "jun", "jul", "aug",
                                         "sep", "oct", "nov", "dec")))

## point patterns

ggplot(earnings, aes(x=height, y=earnings)) + 
  geom_point() + 
  geom_smooth(method=lm, color='orangered')

ggplot(earnings, aes(x=height, y=earnings)) + 
  geom_point(alpha = .1) + 
  geom_smooth(method=lm, color='orangered')

ggplot(earnings, aes(x=height, y=earnings)) + 
  geom_jitter(alpha=.1) + 
  geom_smooth(method=lm, color='orangered')

g = ggplot(earnings, aes(x=height, y=earnings)) + 
  geom_point(alpha = .25) + 
  geom_smooth(method=lm, color='orangered')

ggExtra::ggMarginal(g)

ggplot(earnings, aes(x=height, y=earnings)) +
  geom_smooth(method=lm, color='orangered') + 
  geom_rug()

ggplot(earnings, aes(x=height, y=earnings)) + 
  geom_bin2d(bins=20) + 
  geom_smooth(method=lm, color='orangered')

ggplot(earnings, aes(x=height, y=earnings)) + 
  geom_hex(bins=20) + 
  geom_smooth(method=lm, color='orangered')

ggplot(earnings, aes(x=height, y=earnings)) + 
  geom_density2d_filled() + 
  geom_point(color='white', size=0)



## density a few ways
wecalong = weca %>% pivot_longer(price_dec_1995:price_dec_2018, 
                                 names_prefix='price_', names_sep='_', 
                                 names_to=c("quarter", "year"),
                                 values_to='price')

wecasummary = wecalong %>% group_by(year) %>% 
  summarize(p25 = quantile(price, .25, na.rm=T), 
            p50 = quantile(price, .5, na.rm=T), 
            p75 = quantile(price, .75, na.rm=T)) %>%
  mutate(iqr = p75 - p25, year = as.numeric(year))

ggplot(wecasummary) + 
  geom_ribbon(aes(x=year, ymin=p25, ymax=p75), 
              color='grey', alpha=.4) 

ggplot(wecasummary) + 
  geom_errorbar(aes(x=year, ymin=p25, ymax=p75)) +
  geom_point(aes(x=year, y=p50))

ggplot(wecasummary, aes(x=year, y=p50)) + 
  geom_hex(data=wecalong, 
           mapping=aes(x=as.numeric(year), y=price),
           bins=23) + labs(x='Year', y="Price")

ggplot(wecalong, aes(x=as.numeric(year), group=year, y=price)) + 
  geom_errorbar() 

## comparing distributions

ggplot(earnings, aes(x=earnings, group=race, color=race)) + 
  geom_histogram() + 
  facet_grid(race~., scales = 'free_y')

ggplot(earnings, aes(x=earnings, fill=race)) + 
  geom_density(alpha=.5, lwd=.25)

ggplot(earnings, aes(y=race, x=earnings, fill=race)) + 
  geom_boxplot()

ggplot(earnings, aes(y=race, x=earnings, fill=race)) + 
  geom_violin()

library(ggridges)

ggplot(earnings, aes(y=race, x=earnings)) +
  geom_density_ridges()

## polar plotting for cyclical data

ggplot(temps, aes(x=month, y=temperature, group=year)) +
  geom_line(alpha=.1) +
  geom_smooth(mapping=aes(group=NA), 
              color='orangered')

ggplot(temps, aes(x=month, y=temperature, group=year)) +
  geom_line(alpha=.1) +
  geom_smooth(mapping=aes(group=NA), color='orangered') +
  coord_polar()

## the importance of aspect & contrast

ggplot(temps, aes(x=year, y=temperature)) + 
  geom_point() + 
  geom_smooth(color='orangered') +
  scale_y_log10()

ggplot(temps, aes(x=year, y=temperature, group=year)) + 
  geom_boxplot()
