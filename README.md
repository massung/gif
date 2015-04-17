# GIF Decoder for LispWorks

A simple GIF image decoder in LispWorks.

## Quickstart

A GIF object is created using either the `read-gif` or `load-gif` functions:

	(read-gif input-stream)
	(load-gif pathname)

Once you have a GIF object, you can create a graphics port image using the `make-gif-image` function:

	(make-gif-image port gif &optional data-block overlay merge-p)

***data-block*** is one of the image data blocks within the GIF object. Each data-block represents a portion of the overall image. If *data-block* is nil, then the image will be built by writing all the data blocks in the GIF to a single image.

***overlay*** is the previous image that should be used as the start for the *data-block* to be written to. Usually this is only valuable when creating animation frames, because each frame builds on the previous one.

***merge-p*** should be T if a new image should not be returned, but the *overlay* image should be written to directly.

If you'd like to create an animated GIF object, knowing that the GIF is animated, call `make-gif-animation`.

	(make-gif-animation port gif)

If you'd like to know if a GIF is animated, call `animated-gif-p` on the GIF object. It's possible this returns a false-positive. It simply checks for the "NETSCAPE" application extension in the GIF.
