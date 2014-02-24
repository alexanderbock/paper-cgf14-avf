function [h n] = dcmap2(I, L)
%DCHIST Depth Complexity Histogram

if (size(I, 3) ~= 3)
    error('dchist:numberOfSamples', 'Input image must be RGB.')
end

L2 = [1 L];
s = size(L2);
m = s(2);
n = m+1;

mymap = colormap(hot(256));
mymap(1,:) = [1 1 1];

h = zeros(1,n+1);
img2 = I(:,:,1);
for i = 1:m-1
	h(i) = figure;
    clf;
    set(gcf,'visible','off');
	img = I(:,:,1);
	img(img <= L2(i)) = 0;
	img(img > L2(i+1)) = 0;
	img(img > 1) = 255;
	img2((img2 > L2(i)) & (img2 <= L2(i+1))) = L2(i+1);
	rgb = zeros(size(I));
	rgb(:,:,2) = img;
	imshow(rgb);
end
h(m) = figure;
set(gcf,'visible','off');
image(I(:,:,1));
colormap(mymap);
h(n) = figure;
set(gcf,'visible','off');
image(img2);
colormap(mymap);
