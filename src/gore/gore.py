#!/usr/bin/env python3
"""gore produces spherical gore patterns for balloon construction."""
from PIL import Image
import argparse
import numpy as np


def gore_pattern(r_m, n):
    """gore_pattern generates a gore for sphere construction.

    :param r: The radius of the balloon in meters
    :param n: The desired number of gores
    """
    # The follow formula is derived from first principles. It is
    # the formula for the top half of a full gore. The gore equation
    # is valid between the ranges +-(pi r) / 2 inclusive.
    #
    # g(r, n, x) = +- (pi * r * cos(x / r)) / n
    # r is the radius in meters
    # n is the number of gores
    # x is the value between +- (pi r) / 2

    lower_range = -(np.pi * r_m) / 2.
    upper_range = (np.pi * r_m) / 2.
    total_range = upper_range - lower_range

    # Basic properties of the sphere being formed
    circumfrence = 2. * np.pi * r_m
    surfacearea = 4. * np.pi * r_m ** 2.

    # The total range should be equal to half the circumfrence
    assert (np.allclose(np.abs(total_range - (circumfrence / 2.)), 0))

    # Sample the upper curve at 0.1 meter increments
    xs = np.arange(lower_range, upper_range, 0.00001)
    upper_gore = (np.pi * r_m * np.cos(xs / r_m)) / n

    # The area of the gore times the number of gores must be equal
    # to the surface area of the desired sphere. Since the gore
    # is symmetric along the x axis it is suffience to only integrate
    # once side and multiply by 2.
    gore_area = 2. * np.trapz(upper_gore, xs)

    # Surface area tolerance should be better than manufacturing
    # limits. We use a tolerance of 0.000254 meters or 0.01 inches
    assert (np.allclose(gore_area * n - surfacearea, 0.,
                        atol=0.000254))

    # The xs are symmetric around zero, but a negative distance
    # doesn't make sense in this context so the xs are centered such
    # that the first point is at 0
    return xs - xs[0], upper_gore


def gore_to_ppm(dpi, xs_m, gore_m):
    """gore_to_ppm convers gores to printable patterns.

    :param dpi: The dpi (dots per inch) of the printer
    :param xs_m: The xs sampled in meters, this is the first returned
    element of the gore_pattern function.
    :param gore_m: The upper half of the gore distance from the
    centerline. This is also the second returned paramter of the
    gore_pattern function
    """
    INCHES_IN_METER = 39.37

    # Convert dpi to dots per meter since xs_m and gore_m are in
    # meters.
    dots_per_meter = dpi * INCHES_IN_METER

    # Find the maximum meters in the gore
    gore_max = np.max(gore_m)

    # How many dots are needed to account for the gore width, and
    # gore height.
    gore_height_pixels = (2. * np.ceil(gore_max * dots_per_meter)).astype(int)
    gore_height_pixels += 1

    gore_midline = gore_height_pixels // 2
    gore_width_pixels = ((xs_m[-1] - xs_m[0]) * dots_per_meter).astype(int) + 1

    template = np.ones(shape=(gore_height_pixels, gore_width_pixels),
                       dtype=np.uint8)
    template[:] = 255

    print(f"generating a {template.shape} pixels template...",
          end='', flush=True)

    # Mark pixels as black
    for x_idx, x_m in enumerate(xs_m):
        x_dot = (x_m * dots_per_meter).astype(int)
        y_dot = (gore_m[x_idx] * dots_per_meter).astype(int)
        template[y_dot + gore_midline, x_dot] = 0
        template[gore_midline - y_dot, x_dot] = 0
    print("OK")

    return template


# Perform agument parsing
parser = argparse.ArgumentParser()
parser.add_argument("-r", "--radius", required=True, type=float,
                    help="Radius of the desired balloon")
parser.add_argument("-d", "--dpi", required=True, type=int,
                    help="Dpi to match the printer")
parser.add_argument("-g", "--gores", required=True, type=int,
                    help="Number of gores")
parser.add_argument("-o", "--output", required=False, type=str,
                    default="template.jpg",
                    help="Output file for the template")
args = parser.parse_args()

print("="*70)
print(f"BALLOON RADIUS: {args.radius:16.2f} M")
print(f"SEGMENTS:       {args.gores:16d}")
print(f"DPI:            {args.dpi:16d}")
print("="*70)
print()

# Construct the template
xs_m, upper_gore_m = gore_pattern(args.radius, args.gores)
template = gore_to_ppm(args.dpi, xs_m, upper_gore_m)

# Convert template to jpeg image
im = Image.fromarray(template)
im.save(args.output)
