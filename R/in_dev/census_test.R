directory <- "census_2010_10%_sample"

census_df <- 
list.files(directory, pattern = "csv")%>%
  map_dfr(~.x%>%
            paste0(directory, "/", .)%>%
            data.table::fread(sep = ";")
            #filter(between(c1_00_momloc, 1, 7))
  )

fc_df <- 
  census_df%>%
    filter(between(c1_00_momloc, 1, 7))

rm(census_df)
gc()



pot_twins <- 
fc_df%>%
  select(b1_01_personid, b1_02_householdid, l1_03_4_age_mod)%>%
  mutate(birth_year=2010-l1_03_4_age_mod)%>%
  group_by(b1_02_householdid)%>%
  filter(length(unique(birth_year))<length(unique(b1_01_personid)))


t <- 
pot_twins%>%
  inner_join(pot_twins%>%
                group_by(b1_02_householdid, birth_year)%>%
                count()%>%
                filter(n==1)%>%
                select(b1_02_householdid, birth_year),
             by=join_by(b1_02_householdid, birth_year)
  )%>%
  inner_join(pot_twins%>%
               group_by(b1_02_householdid, birth_year)%>%
               count()%>%
               group_by(b1_02_householdid)%>%
               filter(n>1 & birth_year==max(birth_year))%>%
               ungroup()%>%
               filter(n==2)%>%
               group_by(b1_02_householdid)%>%
               filter(length(unique(birth_year))==1)%>%
               select(b1_02_householdid, birth_year)%>%
               rename(twin_year=birth_year),
             by=join_by(b1_02_householdid)
  )%>%
  filter(birth_year<twin_year)%>%
  filter(between(twin_year-birth_year, 0, 10))%>%
  filter(birth_year>=1970)%>%
  filter(between(l1_03_4_age_mod, 19, 25))


t%>%
  group_by(b1_02_householdid)%>%
  count()%>%
  group_by(n)%>%
  count()
