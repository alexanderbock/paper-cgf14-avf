function [dchValues, numValues] = dchist(I, bins)
%DCHIST Depth Complexity Histogram

if (size(I, 3) ~= 3)
    error('dchist:numberOfSamples', 'Input image must be RGB.')
end

theHist = imhist(I(:,:,1), bins);
dchValues = find(theHist);
dchValues = dchValues - 1;
numValues = theHist(find(theHist ~= 0));

