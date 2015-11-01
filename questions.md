### Questions
Some questions to trigger discussions and considerations while changing the processing parameters, and comparing the quantitative results. Note that not all metrices are applicable for each image.

#### Cross
- How does the gauss filter affect the sizze of the thresheld cross?
- Which filtered removes the noise best, while preserving the cross' shape?
- Can you estimate the length of the edges? Hint: Is there a way enhance the edges? Which metric makes most sense?
- How can you detected all the edges?

#### Dots
- Which filter is best for this kind of noise?
- How do different thresholds affect the paticel size?
- How does a gaussian and median filter affect the particel size and circularity at the same threshold?

#### Blobs
- What might be a problem with this image with respect to thresholding?
- Compare mean, median and gaussian. What are their advantages and disadvantages with respect to object intensity, size and shape?

#### Cells
- Assuming you are not interested in small or weak objects. Which filter will work best?
- What problems occur with the LoG when thresholding, even though we are looking atround objects?

#### Tree
- How many rings are there? How and why do the results differe between LoG and EdgeX/EdgeY?
- Can you say something about their alignment?

#### Breast
Do your best to find the calcifications ;)