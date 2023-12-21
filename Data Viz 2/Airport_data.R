flight_data <- read_csv("nycflights13.csv")
flight_data = rename(flight_data,Flight.Number=rownames,Year = year,Month = month,Day = day,Hour = hour,Dew.Point = dewp,Wind.Direction = wind_dir,Humidity = humid,Wind.Direction = wind_dir,Wind.Speed = wind_speed,Wind.Gusts = wind_gust,Precipitation = precip,Air.Pressure = pressure,Visibility = visib,Time =time_hour,Airport=origin,Temperature_F=temp)
flight_data=transform(flight_data, Temperature_C = (Temperature_F-32)*(5/9))
flight_data=flight_data |>distinct()
flight_data |> arrange(desc(Precipitation)) |> filter(Month == 7 & Precipitation>0 & Airport == 'EWR')
flight_data |> 
  group_by(Airport,Month) |> summarize(
    Avg_Temp_F = mean(Temperature_F)
  )
ggplot(flight_data, aes(x = Air.Pressure, y = Wind.Speed)) +labs(
  title = "Relationship of Air Pressure and Wind Speed for NYC Airports")
geom_point(aes(color = Airport))
ggplot(flight_data, aes(x = Airport)) +   labs(
      title = "Total Flights For Each NYC Airport")+ geom_bar()
ggplot(flight_data, aes(x = Temperature_C)) +
      geom_histogram(binwidth = 10)
ggplot(flight_data, aes(x = Airport, y = Humidity)) +
      geom_boxplot()
ggplot(flight_data, aes(x = Temperature_C, y = Wind.Speed)) +
  facet_wrap(~Airport)
