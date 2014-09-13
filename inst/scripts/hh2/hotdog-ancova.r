
data(hotdog, package="HH")

## constant line across all groups
## y ~ x
ancovaplot(Sodium ~ Calories, groups=Type, data=hotdog)

## different horizontal line in each group
## y ~ a
ancovaplot(Sodium ~ Type, x=Calories, data=hotdog)

## constant slope, different intercepts
## y ~ x + a  or  y ~ a + x
ancovaplot(Sodium ~ Calories + Type, data=hotdog)

## different slopes, and different intercepts
## y ~ x * a  or  y ~ a * x
ancovaplot(Sodium ~ Calories * Type, data=hotdog)
