%Program for Depth Complexity Histograms

%Clear Memory & Command Window
clc;
clear all;
close all;

set(gca,'FontSize',13)

dc_image_folder = 'DCimages\';
dc_hist_folder = 'DChistograms\';
numBins = 256;
maxAxis = 64;

folderL = length(dc_image_folder)+1;

%cd(dc_image_folder);
files = getAllFiles(dc_image_folder);
[m,n] = size(files);

outPath = strcat(pwd, '\', dc_hist_folder);

vBins = [256 256 256 256];
vAxis = [256 128  64  32];

myDCH = zeros(1,256);

for k=1:4
%Start New Calculation of DCH
for j=1:m
    %Read Image
    thisImg = imread(strcat(pwd, '\', files{j}));
    thisSize = size(thisImg);
    
    %Create Depth Complexity Histogram
    [dch,val] = dchist(thisImg, vBins(k));
    %dch
    %val
	max = dch(numel(dch));

    %Parse filename
    fileName = char(files{j});
    fileName = fileName(folderL:length(fileName)-4);
    
    outName = strcat(fileName, '_dch-', int2str(vAxis(k)), '-max', int2str(max));
    mkdir(strcat(outPath, fileName, '\'));
    outFull = strcat(outPath, fileName, '\', outName)

    %CSV
    myDCH(dch+1) = val;
    strDCH = mat2str(myDCH);
    strDCH = strrep(strDCH,'[','');
    strDCH = strrep(strDCH,']','');
    strDCH = strrep(strDCH,' ',', ');
    %Print
    fid2 = fopen(strcat(outFull, '.csv'), 'w+t');
    fprintf(fid2, strcat('# view dependent DCHs\n\n'));
    fclose(fid2);
    fid2 = fopen(strcat(outFull, '.csv'), 'a+t');
    fprintf(fid2, strcat('DCH256', ',\t', strDCH,'\n'));
    fclose(fid2);
    
    %Create Plot
    h = figure;
    set(gcf,'visible','off');
    thisBar = bar(dch,val);
    set(gca, 'xLim', [0 vAxis(k)]); 
    set(gca, 'yScale', 'log'); 
    % Next we plot to SVG add a title, ylabel, grid, box on
    cd('../plot2svg');
    ylabel('Number of pixels (log)', 'FontSize', 20);
    xlabel('Depth Complexity', 'FontSize', 20);
    % Write to folder
    plot2svg(strcat(outFull, '.svg'),h);
    
    cd('../depthcomplex');
            
end % End For Loop
end


