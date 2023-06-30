# Gradient Descent

This is a simple implementation of a Gradient Descent in Haskell for the video of
[Intro - The Math of Intelligence](https://www.youtube.com/watch?v=xRJCOz3AfYY).

## What is it_?
Gradient Descent is an optimization technique to find **local minima** of a function.

## Example
### Data
For this example, I will use the data inside `data/data.csv`. It contains:

|Meters run|Calories lost|
|32.502345269453031|31.70700584656992|
|53.426804033275019|68.77759598163891|
|61.530358025636438|62.562382297945803|
|47.475639634786098|71.546632233567777|

### Line formula
This is the formula of a straight line:

```haskell
y = m x + b
```

Where, `m` is the **slope** and `b` is the **intercept point** of our codomain.

### Error messure
This is the measue of closeness which is defined for this example as the sum of 
squares error:

```haskell
sse = sum (y_i - y) ** 2
```
