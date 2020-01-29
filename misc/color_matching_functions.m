% color_matching_functions -- process color matching functions.
%
%      color_matching_functions(id)
%
% Process color matching functions.
%
% Argument ID is either 1931, i.e. the CIE 1931 standard observer,
% or 1964, i.e. the CIE 1964 standard observer.

%% color_matching_functions.m --- the preceding comment is the documentation string.

% Copyright (C) 2020 Ralph Schleicher

% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions
% are met:
%
%    * Redistributions of source code must retain the above copyright
%      notice, this list of conditions and the following disclaimer.
%
%    * Redistributions in binary form must reproduce the above copyright
%      notice, this list of conditions and the following disclaimer in
%      the documentation and/or other materials provided with the
%      distribution.
%
%    * The name of the author may not be used to endorse or promote
%      products derived from this software without specific prior
%      written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS
% OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
% INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
% STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
% IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.

%% Code:

% Program entry point.
function color_matching_functions(id)

  if nargin < 1
    color_matching_functions(1931);
    color_matching_functions(1964);
    return;
  end

  switch id
   case {1931, 1964}
    from = sprintf('204_%d_col_observer.csv', id);
    to = sprintf('cie_%d_standard_observer.txt', id);
   otherwise
    error('Unknown identifier.');
  end

  % Input data with a step size of 5 nm.
  d5 = csvread(from);

  % Interpolate with a step size of 1 nm.
  d1l = interp(d5, 380:780, 'linear');
  d1c = interp(d5, 380:780, 'pchip');
  d1s = interp(d5, 380:780, 'spline');

  % Output data with a step size of 1 nm in the range from LOW to HIGH.
  d1 = d1s;

  switch id
   case 1931
    low = 450;
    high = 575;
   case 1964
    low = 465;
    high = 560;
  end

  % Merge 5 nm and 1 nm data sets.
  d51 = sortrows([d5(d5(:, 1) < low+1 | d5(:, 1) > high-1, :); ...
                  d1(d1(:, 1) > low   & d1(:, 1) < high  , :)], 1);

  % Save it.
  c = cell(size(d51));
  c(:, 1) = num2cell(d51(:, 1));
  c(:, 2) = format(d51(:, 2));
  c(:, 3) = format(d51(:, 3));
  c(:, 4) = format(d51(:, 4));

  h = fopen(to, 'w');
  if h == -1
    error('Can not open file ''%s'' for writing.', to);
  end
  for i = 1:size(c, 1)
    fprintf(h, '%3d %s %s %s\n', c{i, :});
  end
  if fclose(h) == -1
    error('Can not close file ''%s''.', to);
  end

  % Visualization.
  switch 0
   case 1
    [x5, y5] = cie_xy(d5);
    [x1l, y1l] = cie_xy(d1l);
    [x1c, y1c] = cie_xy(d1c);
    [x1s, y1s] = cie_xy(d1s);
    [x51, y51] = cie_xy(d51);

    figure(1);
    plot(x5, y5, 'k.', ...
         x1l, y1l, 'r', ...
         x1c, y1c, 'g', ...
         x1s, y1s, 'b', ...
         x51, y51, 'm');
    for k = 1:numel(x5)
      text(x5(k), y5(k), sprintf(' %d ', d5(k, 1)));
    end

    % Distance to linear interpolation.
    r1l = hypot(x1l - x1l, y1l - y1l);
    r1c = hypot(x1c - x1l, y1c - y1l);
    r1s = hypot(x1s - x1l, y1s - y1l);

    % Distance to CIE standard illuminant E.
    a1l = hypot(x1l - 1/3, y1l - 1/3);
    a1c = hypot(x1c - 1/3, y1c - 1/3);
    a1s = hypot(x1s - 1/3, y1s - 1/3);

    figure(2);
    subplot(3, 1, 1);
    plot(d1l(:, 1), r1l, 'r', ...
         d1c(:, 1), r1c, 'g', ...
         d1s(:, 1), r1s, 'b');
    title('Distance to Linear Interpolation');
    subplot(3, 1, 2);
    plot(d1l(:, 1), a1l, 'r', ...
         d1c(:, 1), a1c, 'g', ...
         d1s(:, 1), a1s, 'b');
    title('Absolute Distance to CIE standard illuminant E');
    subplot(3, 1, 3);
    plot(d1l(:, 1), (a1l ./ a1l - 1) .* 100, 'r', ...
         d1c(:, 1), (a1c ./ a1l - 1) .* 100, 'g', ...
         d1s(:, 1), (a1s ./ a1l - 1) .* 100, 'b');
    title('Relative Distance to CIE standard illuminant E');
    ylabel('%');
    linkaxes(get(gcf, 'Children'), 'x');
  end

% Interpolate data set.
function d1 = interp(d5, xi, method)

  x = d5(:, 1);
  y = d5(:, 2:4);

  xi = xi(:);
  yi = interp1(x, y, xi, method);

  d1 = [xi, yi];

% Format numbers.
function str = format(num)

  % Need FLT_DIG+1 significant digits for reading NUM as
  % single-precision floating-point numbers.
  str = arrayfun(@(a) sprintf('%.7G', a), num, 'UniformOutput', false);

  % If the G conversion uses exponential notation, ensure
  % there is a decimal point.
  k = cellfun(@(c) any(c == 'E'), str);
  % Remove leading zeros in exponent.
  str(k) = regexprep(str(k), '(E[-+])0+', '$1');
  % Insert decimal point.
  k = k & cellfun(@(c) all(c ~= '.'), str);
  str(k) = strrep(str(k), 'E', '.0E');

  % Add a decimal point to integers, too.
  k = cellfun(@(c) all(c ~= '.'), str);
  str(k) = cellfun(@(c) [c, '.0'], str(k), 'UniformOutput', false);

  % Align numbers at the decimal point.
  k = cellfun(@(c) min([find(c == '.'), find(c == 'E'), numel(c) + 1]), str);
  pad = arrayfun(@(a) repmat(' ', 1, a), max(k) - k, 'UniformOutput', false);
  str = arrayfun(@(k) [pad{k}, str{k}], (1:numel(str)).', 'UniformOutput', false);

  % Equal length.
  k = cellfun(@numel, str);
  pad = arrayfun(@(a) repmat(' ', 1, a), max(k) - k, 'UniformOutput', false);
  str = arrayfun(@(k) [str{k}, pad{k}], (1:numel(str)).', 'UniformOutput', false);

% Chromaticity coordinates.
function [x, y] = cie_xy(dat)

  tem = sum(dat(:, 2:4), 2);

  x = dat(:, 2) ./ tem;
  y = dat(:, 3) ./ tem;

%% color_matching_functions.m ends here
