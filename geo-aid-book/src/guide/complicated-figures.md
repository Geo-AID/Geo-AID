# Dealing with complicated figures

Sometimes you will stumble on a figure that is quite challenging to draw, even for Geo-AID. In these cases, it's worth
knowing a few tricks to guide Geo-AID through them.

When Geo-AID generates a figure, it does so with a certain amount of samples, over multiple cycles, until a certain
condition (currently: average quality delta over the last x cycles goes below a certain value) is met. That's a lot of
different generation parameters and all of them are modifiable.

1. Sample count (`-s` or `--samples` option, 512 by default)

By modifying this parameter, you change how many different versions of a generation cycle are created. The higher the
value, the more likely Geo-AID is to find the right spot for every point. It will, however, take more time, and it might
make certain flaws of Geo-AID's generation more visible.

2. Generation break average delta threshold (`-d` option, 0.0001 by default)

Lowering this makes Geo-AID go on with generation for a little longer, essentially postponing the moment it decides it
won't really get much better.

3. Count of last records used in calculating the average delta (`-m` option, 128 by default)

Increasing this makes Geo-AID take more of the last cycles into consideration when deciding whether to stop.

4. Generation engine (`-e` or `--engine`, `glide` by default)

By default, Geo-AID uses `glide` as its optimization engine as it should generally perform better than `rage`. It might,
however, be worth a try to switch the engine.

5. Maximum adjustment per generation cycle (`-a` option, 0.5 by default)

This modifies how much can a single point/scalar be changed when adjusting for another cycle. Keep in mind that this is
only a base for calculations. In reality, the amount of adjustment allowed depends on the given point's quality and is
different between different workers to allow both big *and* small changes in the same generation cycle. This parameter
only works with the Rage engine.

---

Usually, the most visible effect comes from increasing the sample count.

Ultimately the best way of increasing odds for Geo-AID is to write the script as well as you can, relying strongly on
defining points with expressions. The golden rule is: the fewer rules, the better.