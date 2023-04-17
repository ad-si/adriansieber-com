+++
title = "Visualize Utility Meter Readings With Elixir's Livebook"
author = "Adrian Sieber (ad-si.com)"
draft = true

[taxonomies]
tags = ["programming", "elixir", "livebook", "visualization"]
+++

## Start with Docker

```sh
docker run \
  -p 8080:8080 \
  -p 8081:8081 \
  --pull always \
  -u $(id -u):$(id -g) \
  -v $(pwd):/data \
  livebook/livebook
```


## Install Dependencies

```elixir
Mix.install([
  {:kino, "~> 0.6.1"},
  {:vega_lite, "~> 0.1.5"},
  {:kino_vega_lite, "~> 0.1.1"}
])

alias VegaLite, as: Vl
```


## Water

Water readings in $m^3$.

```elixir
waterReadings = [
  %{"utc" => "2020-07-31 19:10", "value" => 571.02},
  %{"utc" => "2021-10-23 14:37", "value" => 652.45},
  %{"utc" => "2022-06-07 09:26", "value" => 723.59}
]

Vl.new(width: 600, height: 300)
|> Vl.data_from_values(waterReadings)
|> Vl.mark(:line)
|> Vl.encode_field(:x, "utc", type: :temporal, axis: [label_angle: 45])
|> Vl.encode_field(:y, "value", type: :quantitative, scale: [zero: false])
```


## Electricity

Electricity readings in $kWh$.

```elixir
electricityReadings = [
  %{"utc" => "2020-07-31 19:09", "value" => 65532.80},
  %{"utc" => "2021-10-23 14:37", "value" => 67962.19},
  %{"utc" => "2022-07-28 22:56", "value" => 69538.37}
]

Vl.new(width: 600, height: 300)
|> Vl.data_from_values(electricityReadings)
|> Vl.mark(:line)
|> Vl.encode_field(:x, "utc", type: :temporal, axis: [label_angle: 45])
|> Vl.encode_field(:y, "value", type: :quantitative, scale: [zero: false])
```


## Gas

Gas readings in $m^3$.

```elixir
gasReadings = [
  %{"utc" => "2020-07-31 19:06", "value" => 6871.93},
  %{"utc" => "2021-10-23 14:37", "value" => 7814.82},
  %{"utc" => "2022-07-07 10:10", "value" => 8503.08}
]

Vl.new(width: 600, height: 300)
|> Vl.data_from_values(gasReadings)
|> Vl.mark(:line)
|> Vl.encode_field(:x, "utc", type: :temporal, axis: [label_angle: 45])
|> Vl.encode_field(:y, "value", type: :quantitative, scale: [zero: false])
```
