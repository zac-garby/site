---
title: Procedurally generating maps
banner: /images/territories-banner.png
---

First off, I know haven't written a post on here since 2019. How do you like the new logo? It's only *semi* ironic.

# Some background

So recently I was thinking it would be cool to make a game a bit like Risk or [Warzone](https://www.warzone.com), but with some slightly different gameplay rules and balancing to give the losing players at least a fighting chance. I had a bunch of ideas on how this could work (country population, local deployment, army upkeep, etc etc), but then I made the unfortunate decision that, "hey, randomly generated maps could be cool!"

This was only an unfortunate decision in the sense that I got so distracted by the territory generation that I completely forgot to actually make the game itself. Oh well, maybe I'll come back to it some day and finish it off. Either way, the procedural generation algorithm I came up with was pretty cool so I thought I'd talk about it here.

# The algorithm

My goal was basically to get some random-looking shapes which tesselate together in the plane. Right now, I've just focussed on generating these regions inside a rectangular "continent", although if I wanted to make this into an actual game I'd probably generate some funny-shaped continents like in real life and then fill them with regions individually. The algorithm I came up with should adapt quite easily to filling in an irregular shape, luckily.

## Voronoi

Right away, this "random-looking shapes which tesselate" sounds an awful lot like what a [Voronoi diagram](https://en.wikipedia.org/wiki/Voronoi_diagram) would give me. A Voronoi diagram is what you get when you place some "seeds" on a plane and then colour every point based on which seed it's closest to. Selecting 30 random points and running Voronoi on every pixel gets me this:

![](/images/territories-1.png)

I mean... It's not awful, but I haven't seen many countries in real life which have such flat, straight edges. It looks a bit unnatural, and I think we can do better. My first thought here was to use a different distance metric. The distance metric defines how the distance between any two points is calculated. For my first try, I'd defaulted to [Euclidean distance](https://en.wikipedia.org/wiki/Euclidean_distance) (which, you know, fair enough. We probably live in a Euclidean universe after all.) This is where the distance between two points $(x_1, y_1)$ and $(x_2, y_2)$ is defined as $\sqrt{(x_2-x_1)^2 + (y_2-y_1)^2}$.

One alternative to Euclidean distance is [Manhattan distance](https://en.wikipedia.org/wiki/Taxicab_geometry), also known as Taxicab geometry. This is defined as the shortest distance you could go to each the other point if you're only allowed to move north, west, south, or east, and is named after how boring the street layout in Manhattan is.

For 2-dimensional coordinates, it's defined as $|x_2 - x_1| + |y_2 - y_1|$, where the vertical lines represent the absolute value. Using this as the distance metric instead of Euclidean, I get this:

![](/images/territories-2.png)

It certainly looks a bit more interesting and realistic, and is starting to actually look like countries rather than a mathematical diagram. There are still obviously problems, though. No country has borders as rigid and linear as these ones, and all the lines are either axis-aligned or at a 45° angle. Not very realistic. Real countries like their weird angles.

By the way, I'm using the same seed for each of these generations, so you can see the differences when starting with the same points.

## A Better Distance Metric

So at this point I'm pretty sure that voronoi is the way to go, but none of these standard distance metrics give a natural, smooth, organic-looking border. Really want I wanted was some kind of wavy, bendy borders between the regions, and ideally in a non-uniform way (so not all borders would be wavy to the same degree.)

The problem is, there's no obvious way to make the borders wavy *after* they've been generated, which means that probably the way to go here is to come up with a new distance metric which produces these nice borders without any extra work.

So far for the two distance metrics I've looked at - Euclidean and Manhattan - I've basically assumed that the ground is flat ([*cough cough*](https://www.tfes.org)). This is obviously not the case in real life, and not only because it's spherical. There are mountain ranges, valleys, rivers, hills, and many other non-flat components. If I were to take into account of the third axis when calculating distance, after generating some sufficiently bumpy mountains all over the place, then surely the borders between the regions should become wobbly.

The distance can't simply be the Euclidean or Manhattan distance in three dimensions, though. Most humans can't fly, and so a straight line to a mountain peak is not possible. Instead, the slopes must be traversed, and so any falls and rises in between need to be accounted for. The proper way to do this would be to find the length of the curve which cuts through the direction we want to move in, but since I'm doing this on a computer, I'll need to do it numerically.

![](/images/territories-3.png)

This image shows how the length of the curve can be approximated. Basically, it's split into a number of chunks, each being a trapezium. Then, the lengths of the top edge of each chunk are added together, calculated using Pythagoras. This total length is then used as the distance, giving a nice wobbly distance function.

One thing to note is the size of the chunks. I first tried having them all the same size, 5 pixels, but that meant that the distance function was non-continuous, giving some weird artefacts around the borders. It also meant that it was very slow, because two points very far away would have to evaluate hundreds of Pythagorases. Instead, I decided to use a fixed *amount* of chunks, and have the length of each one just the total length to travel divided by this number.

But, you may be asking, "How do we actually generate this real-looking terrain to calculate the rises and falls???"

## Perlin Noise

So as you probably know if you've ever looked into terrain generation, [Perlin noise](https://en.wikipedia.org/wiki/Perlin_noise) is a very common way of getting some natural-looking bumpy terrain-like noise. It looks something like this.

![[From Wikipedia Commons, rescaled and cropped](https://commons.wikimedia.org/wiki/File:Perlin_noise.jpg)](/images/territories-4.png)

If you add a bunch of these on top of each other, scaled up and down to get different frequencies, you get what's called "fractal noise". It's a bit less predictable and blobby than the pure Perlin noise but still looks natural. This will look a bit like this:

![[From Wikipedia Commons, rescaled and cropped](https://commons.wikimedia.org/wiki/File:2D_sample_of_Perlin_noise.png)](/images/territories-5.png)

If we use this as a heightmap, along with the distance metric I described a minute ago, the distance between two points corresponds to the distance that you would have to walk to reach one point from another if you could only walk in one direction - towards it.

Although to be honest, this physical analogy isn't that important here. All that matters really is that the distance function has some noise incorporated into it, and the noise is coherent and consistent at each point. I could probably just as well have added a noise function to Euclidean distance, where the noise is sampled at the midpoint of the two endpoints, *but* this is cooler so I'm going with it.

Anyway, applying this distance metric to the Voronoi described before, I got this output:

![](/images/territories-6.png)

This looks really cool in my opinion! Most of these regions look like they could be perfect little countries. The shapes are nice and irregular and wavy, and there are loads of different shapes (long, short, round, you know - all the shapes.)

Also, because of the use of Perlin noise, there are a whole load of parameters I can tune to change the borders to my liking. For example, I could make the borders more wavy by adding more high-frequency noise, or I could make the general shapes of the countries more irregular by adding more low-frequency noise.

There are a few things that bug me though. Most notably, there are some absolutely tiny regions that probably represent Vatican City and Luxembourg, but I don't really want them there, most of all because it would be impossible to actually overlay a UI over these tiny countries if I were to actually make this into a game.

Another small problem is that the countries are mostly very similar in area (other than these tiny ones.) I guess this makes sense, though, considering the points are uniform-randomly distributed. This would be a nice improvement though, if there could be giant countries like Russia. Not sure how well this would work in the context of a strategy game, though.

## Removing Tiny Regions

As I was saying, tiny countries annoy me. I'm also really not sure why there are so many tiny ones, but basically none in a certain range between these tiny ones and the smallest of the larger ones. It's interesting, but anyway, how can I remove them.

It's simple enough to measure the amount of pixels in each region. Just loop through the whole map and, for each pixel, increment a counter corresponding to the region the pixel is part of. But then, to actually remove a region means I will have to replace its pixels with a different region, otherwise it will just be unclaimed land which isn't really allowed, so which region do I allow to claim it?

My solution to this was, if a region is too small, replace it with the neighbouring region which it shares the largest amount of border with. The details of how this is done are slightly technical but very boring - basically I compute an adjacency matrix where I count how many borders are shared between every pair of regions, and then I can find the minimum of a given row when a region is too small.

This process is repeated until no changes are made, because joining two regions that are too small can still produce a region that's too small. Once it's all run, I got this as my output:

![](/images/territories-7.png)

Pretty good.

# Conclusion

For now, I'm really happy with these results. I think the countries look very realistic, at least to my eyes, and it's a really nice algorithm in my opinion.

Of course, this isn't to say there are no improvements that could be made. There are loads of things that could be done, in fact.

 - Pathfinding for the distance function. If you remember when talking about my distance metric I said that it represents the distance you'd have to walk if you could only go straight towards your target. In real life, people tend to walk around mountains rather than over the top if they can avoid it, so if I applied some kind of pathfinding (probably Floyd's), I could take this into account. Hopefully this might mean that the region boundaries would conform more to the land topology.

   Again, though, this isn't so important. I'm not going for a hyper-realistic country expansion simulation here, I'm just trying to get some mildly realistic-looking borders, so this might be overkill.
  
 - Improve the initial point distribution. A uniform distribution is okay, and gets some nice looking regions, but it's not exactly realistic. In more habitable places, more civilisations are likely to start, and so it would make sense for there to be clumps of points in some areas and relatively fewer in other areas.

   Doing this could give me a nice distribution of region areas, which would definitely look cool but may not work as well in a game, since if one player started off with half the map then they'd pretty much be set.

Anyway that's it for now. I've implemented it [here](https://github.com/zac-garby/territories) so you can have a play around with it if you want.

I'll probably make this into an actual game at some point, as well.