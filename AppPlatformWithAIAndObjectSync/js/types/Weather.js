import {defineType} from '../platform.js';

defineType({
  name: 'Weather',
  storage: 'Weathers',
  display: 'Name',
  properties: {
    Name: { type: 'string' },
    TemperatureC: { type: 'number', label: 'Temperature (°C)' },
    HumidityPercent: { type: 'number', label: 'Humidity (%)' },
    WindKmh: { type: 'number', label: 'Wind (km/h)' },
    WeatherCode: { type: 'integer', label: 'WMO weather code' },
    ObservedUTC: { type: 'string', label: 'Observation time (UTC)' },
    Source: { type: 'string' },
  },
  data: [
    {
      Name: 'New York',
      TemperatureC: 12.6,
      HumidityPercent: 71,
      WindKmh: 12.7,
      WeatherCode: 0,
      ObservedUTC: '2026-10-15T11:00Z',
      Source: 'Open-Meteo',
    },
  ],
});