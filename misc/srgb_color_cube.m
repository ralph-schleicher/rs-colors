% srgb_color_cube -- plot the sRGB color cube.
%
%      srgb_color_cube(...)
%
% Plot the sRGB color cube in different color spaces.

%% srgb_color_cube.m --- the preceding comment is the documentation string.

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
%    * Neither the name of the copyright holder nor the names of its
%      contributors may be used to endorse or promote products derived
%      from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
% FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.

%% Code:

% Program entry point.
function srgb_color_cube(varargin)

  if nargin == 0
    srgb_color_cube -views -save -close
    return;
  end

  if exist('OCTAVE_VERSION', 'builtin')
    warning('off', 'Octave:possible-matlab-short-circuit-operator');
  end

  % Valid color space identifiers.
  tags = {'RGB', 'HSV', 'HSL', 'CIERGB', 'CIEXYZ', 'CIExyY', 'CIELuv', 'CIELab'};

  % Parse arguments.
  opt = [];

  opt.tag = cell2struct(num2cell(false(size(tags))), tags, 2);
  opt.mode = '3d';
  opt.save = false;
  opt.close = false;

  for k = 1:numel(varargin)
    arg = varargin{k};
    if ~ ischar(arg) || ndims(arg) ~= 2 || size(arg, 1) > 1 || size(arg, 2) < 1
      error('Invalid argument.');
    end
    j = find(strcmpi(arg, tags));
    if ~ isempty(j)
      opt.tag.(tags{j}) = true;
    else
      % Argument has to be an option.
      if arg(1) ~= '-'
        error('Unknown color space identifier ''%s''.', arg);
      end
      switch arg
       case {'-3d', '-3'}
        opt.mode = '3d';
       case {'-iso'}
        opt.mode = 'iso';
       case {'-front', '-fron', '-fro', '-fr', '-f'}
        opt.mode = 'front';
       case {'-right', '-righ', '-rig', '-ri', '-r'}
        opt.mode = 'right';
       case {'-back', '-bac', '-ba', '-b'}
        opt.mode = 'back';
       case {'-left', '-lef', '-le', '-l'}
        opt.mode = 'left';
       case {'-top', '-to', '-t'}
        opt.mode = 'top';
       case {'-drawing', '-drawin', '-drawi', '-draw', '-dra', '-dr', '-d'}
        opt.mode = 'drawing';
       case {'-views', '-view', '-vie', '-vi', '-v'}
        opt.mode = 'views';
       case {'-isotop', '-isoto', '-isot'}
        opt.mode = 'isotop';
       case {'-save', '-sav', '-sa', '-s'}
        opt.save = true;
       case {'-no-save', '-no-sav', '-no-sa', '-no-s'}
        opt.save = false;
       case {'-close', '-clos', '-clo', '-cl', '-c'}
        opt.close = true;
       case {'-no-close', '-no-clos', '-no-clo', '-no-cl', '-no-c'}
        opt.close = false;
       otherwise
        error('Unknown option ''%s''.', arg);
      end
    end
  end

  % Read color data.
  head = cell2struct({'sRGB', [], {'R', 'G', 'B'}; ...
                      'RGB', [], {'R', 'G', 'B'}; ...
                      'HSV', [], {'H', 'S', 'V'}; ...
                      'HSL', [], {'H', 'S', 'L'}; ...
                      'CMY', [], {'C', 'M', 'Y'}; ...
                      'CMYK', [], {'C', 'M', 'Y', 'K'}; ...
                      'CIERGB', 'CIE RGB', {'R', 'G', 'B'}; ...
                      'CIEXYZ', 'CIE XYZ', {'X', 'Y', 'Z'}; ...
                      'CIExyY', 'CIE xyY', {'x', 'y', 'Y'}; ...
                      'CIELuv', 'CIE L*u*v*', {'L*', 'u*', 'v*'}; ...
                      'CIELab', 'CIE L*a*b*', {'L*', 'a*', 'b*'}; ...
                      'CIELCh', 'CIE L*C*h', {'L*', 'C*', 'h'}}, ...
                     {'tag', 'name', 'coord'}, 2);
  for k = 1:numel(head)
    if isempty(head(k).name)
      head(k).name = head(k).tag;
    end
  end
  % Column offset.
  head(1).column = 0;
  for k = 2:numel(head)
    head(k).column = head(k - 1).column + numel(head(k - 1).coord);
  end

  data = csvread('srgb-color-cube.csv', 2, 0);

  % Dispatch.
  not_any = ~ any(structfun(@(s) s, opt.tag));

  for k = 1:numel(tags)
    tag = tags{k};
    if opt.tag.(tag) || not_any
      j = find(strcmp({head.tag}, tag));
      srgb_color_cube_r(head(j), data, opt);
    end
  end

function srgb_color_cube_r(head, data, opt)

  % sRGB color values for use with Octave/Matlab.
  C = data(:, 4:6);

  % Color space coordinates.
  V = data(:, head.column + (1:3));

  % Color space coordinates in R³.
  P = V;

  switch head.tag
   case 'HSV'
    % Cone.
    P(:, 1) = cosd(V(:, 1)) .* V(:, 2) .* V(:, 3);
    P(:, 2) = sind(V(:, 1)) .* V(:, 2) .* V(:, 3);
   case 'HSL'
    % Bicone.
    t = V(:, 3);
    i = (t > 0.5);
    t(i) = 1 - t(i);
    P(:, 1) = cosd(V(:, 1)) .* V(:, 2) .* t .* 2;
    P(:, 2) = sind(V(:, 1)) .* V(:, 2) .* t .* 2;
   case {'CIELuv', 'CIELab'}
    P(:, 1:3) = V(:, [2, 3, 1]);
  end

  % Create figure.
  fig = figure('Name', head.tag, ...
               'Color', [1, 1, 1] .* (223/255), ...
               'InvertHardcopy', 'off');

  ax_prop = {'Units', 'centimeters', ...
             'Margin', 0.5, ...
             'Padding', 1, ...
             'Width', 4, ...
             'Height', 4};
  switch opt.mode
   case {'3d', 'iso', 'front', 'right', 'back', 'left', 'top'}
    ax = axes_layout('Rows', 1, 'Columns', 1, ax_prop{:});
    srgb_color_cube_plot(ax, head, V, P, C, opt.mode);

   case 'drawing'
    ax = axes_layout('Rows', 2, 'Columns', 2, ax_prop{:});
    srgb_color_cube_plot(ax(1, 1), head, V, P, C, 'front');
    srgb_color_cube_plot(ax(1, 2), head, V, P, C, 'left');
    srgb_color_cube_plot(ax(2, 1), head, V, P, C, 'top');
    srgb_color_cube_plot(ax(2, 2), head, V, P, C, '3d');

   case 'views'
    ax = axes_layout('Rows', 1, 'Columns', 3, ax_prop{:});
    srgb_color_cube_plot(ax(1), head, V, P, C, '3d');
    srgb_color_cube_plot(ax(2), head, V, P, C, '3d');
    srgb_color_cube_plot(ax(3), head, V, P, C, '3d');
    [az, el] = view(ax(1));
    view(ax(2), az - 120, el);
    view(ax(3), az - 240, el);

   case 'isotop'
    ax = axes_layout('Rows', 1, 'Columns', 2, ax_prop{:});
    srgb_color_cube_plot(ax(1), head, V, P, C, '3d');
    srgb_color_cube_plot(ax(2), head, V, P, C, 'top');
  end

  if opt.save
    saveas(fig, sprintf('srgb-color-cube-%s-%s.png', head.tag, opt.mode));
  end

  if opt.close
    close(fig);
  end

function h = srgb_color_cube_plot(ax, head, V, P, C, mode)

  set(ax, 'Color', 'none', ...
          'Visible', 'off', ...
          'Clipping', 'off', ...
          'FontSize', 7);

  axes(ax);
  view(3);

  % Faces of the sRGB cube in another color space.
  switch head.tag
   case {'HSV', 'HSL'}
    % Copy data for hue = 0 to hue = 360 so that the faces are closed.
    i = (V(:, 1) == 0);
    tem = V(i, :);
    tem(:, 1) = 360;
    V = [V; tem];
    P = [P; P(i, :)];
    C = [C; C(i, :)];
  end

  switch head.tag
   case 'HSV'
    h = gobjects(1, 2);
    % Bottom.
    i = find(V(:, 2) == 1 | V(:, 3) == 0);
    tri = delaunay(V(i, [1, 3]));
    h(1) = patch('Faces', i(tri), 'Vertices', P);
    % Top.
    i = find(V(:, 3) == 1);
    tri = delaunay(V(i, [1, 2]));
    h(2) = patch('Faces', i(tri), 'Vertices', P);
   case 'HSL'
    h = gobjects(1, 2);
    % Bottom.
    i = find((V(:, 2) == 1 | V(:, 3) == 0) & V(:, 3) <= 0.5);
    tri = delaunay(V(i, [1, 3]));
    h(1) = patch('Faces', i(tri), 'Vertices', P);
    % Top.
    i = find((V(:, 2) == 1 | V(:, 3) == 1) & V(:, 3) >= 0.5);
    tri = delaunay(V(i, [1, 3]));
    h(2) = patch('Faces', i(tri), 'Vertices', P);
   otherwise
    h = gobjects(3, 2);
    for dim = 1:3
      for val = 0:1
        i = find(C(:, dim) == val);
        tri = delaunay(C(i, setdiff(1:3, dim)));
        h(dim, val + 1) = patch('Faces', i(tri), 'Vertices', P);
      end
    end
  end

  set(h, 'FaceVertexCData', C, ...
         'FaceColor', 'interp', ...
         'EdgeColor', 'none', ...
         'Clipping', 'off');

  switch head.tag
   case {'HSV', 'HSL'}
    set(ax, 'XLim', [-1, 1], ...
            'YLim', [-1, 1], ...
            'ZLim', [ 0, 1]);
    daspect([2, 2, 1]);
   case {'CIExyY'}
    set(ax, 'XLim', [0, 0.5], ...
            'YLim', [0, 0.5], ...
            'ZLim', [0, 1]);
    daspect([1, 1, 1]);
   case {'CIELuv', 'CIELab'}
    set(ax, 'XLim', [-150, 150], ...
            'YLim', [-150, 150], ...
            'ZLim', [   0, 100]);
    daspect([3, 3, 1]);
   otherwise
    set(ax, 'XLim', [0, 1], ...
            'YLim', [0, 1], ...
            'ZLim', [0, 1]);
    daspect([1, 1, 1]);
  end

  % Freeze aspect ratios.
  axis('vis3d');

  base = 1:3;
  switch mode
   case 'front'
    view(0, 0); % +x-axes
    base(2) = [];
   case 'right'
    view(90, 0); % +y-axes
    base(1) = [];
   case 'back'
    view(180, 0); % -x-axes
    base(2) = [];
   case 'left'
    view(270, 0); % -y-axes
    base(1) = [];
   case 'top'
    view(0, 90); % (x, y) plane
    base(3) = [];
   case 'iso'
    view(30, 30); % isometric
   otherwise
    view(37.5, 22.5);
  end

  switch head.tag
   case {'HSV', 'HSL'}
    if numel(base) == 3 % 3d
      basis(ax, [0, 0, 1, 3/4], 10, 70, 15, head.coord{1});
      basis(ax, [1, 0, 0], 0, 1, 0.15, head.coord{2});
    elseif all(base ~= 3) % top
      basis(ax, [0, 0, 1, 3/4], 10, 70, 15, head.coord{1}, 'Origin', [0, 0, 1]);
      basis(ax, [1, 0, 0], 0, 1.3, 0.15, head.coord{2}, 'Origin', [0, 0, 1]);
    else
      if any(base == 1) basis(ax, [ 1,  0, 0], 0, 1, 0.15, head.coord{2}); end
      if any(base == 1) basis(ax, [-1,  0, 0], 0, 1, 0.15, head.coord{2}); end
      if any(base == 2) basis(ax, [ 0,  1, 0], 0, 1, 0.15, head.coord{2}); end
      if any(base == 2) basis(ax, [ 0, -1, 0], 0, 1, 0.15, head.coord{2}); end
    end
    if any(base == 3) basis(ax, [0, 0, 1], 0, 1.20, 0.075, head.coord{3}); end
   case {'CIExyY'}
    if any(base == 1), basis(ax, [1, 0, 0], 0, 0.65, 0.075, head.coord{1}); end
    if any(base == 2), basis(ax, [0, 1, 0], 0, 0.65, 0.075, head.coord{2}); end
    if any(base == 3), basis(ax, [0, 0, 1], 0, 1.00, 0.075, head.coord{3}); end
   case {'CIELuv', 'CIELab'}
    if any(base == 1), basis(ax, [1, 0, 0], -145, 145, 22.5, head.coord{2}); end
    if any(base == 2), basis(ax, [0, 1, 0], -145, 145, 22.5, head.coord{3}); end
    if any(base == 3), basis(ax, [0, 0, 1],    0, 115,  7.5, head.coord{1}); end
   otherwise
    if any(base == 1), basis(ax, [1, 0, 0], 0, 1.15, 0.075, head.coord{1}); end
    if any(base == 2), basis(ax, [0, 1, 0], 0, 1.15, 0.075, head.coord{2}); end
    if any(base == 3), basis(ax, [0, 0, 1], 0, 1.15, 0.075, head.coord{3}); end
  end

  rotate3d('on');

function basis(ax, e, t1, t2, dt3, str, varargin)

  % Parse property/value pairs.
  [prop, arg] = getprop(struct('Origin', [0, 0, 0]), ...
                        varargin);

  % Direction vector.
  e = reshape(e, 1, numel(e));

  % Radius.
  r = 0;

  switch numel(e)
   case 3
   case 4
    r = abs(e(4));
    e(4) = [];
   otherwise
    error('Fix me.');
  end

  % Unit vector.
  e = e ./ norm(e);

  % Arrow line coordinates.
  v = [e .* t1; ...
       e .* t2];

  % Label position.
  if isempty(dt3)
    t3 = nan;
  else
    t3 = t2 + dt3;
  end
  p = e .* t3;

  if r ~= 0
    a = [linspace(t1, t2, 1 + max((t2 - t1) ./ 7.5, 1)), t3].';
    if e(3) == 1
      % Rotation about z-axis.
      v = [r .* cosd(a), r .* sind(a), zeros(size(a))];
    else
      error('Fix me.');
    end
    p = v(end, :);
    v(end, :) = [];
  end

  % Move origin.
  if any(prop.Origin ~= 0)
    v = bsxfun(@plus, v, prop.Origin);
    p = bsxfun(@plus, p, prop.Origin);
  end

  % Last line segment defines the direction of the arrow head.
  h = arrow3(v(end - 1, :), v(end, :));
  set(h(1), 'XData', v(:, 1), ...
            'YData', v(:, 2), ...
            'ZData', v(:, 3), ...
            'Color', 'k');
  set(h(2), 'FaceColor', 'k', ...
            'EdgeColor', 'k');

  % Text label.
  if ~ (isnan(t3) || isempty(str))
    text(p(1), p(2), p(3), str, ...
         'FontSize', get(ax, 'FontSize'), ...
         'HorizontalAlignment', 'center', ...
         'VerticalAlignment', 'middle', ...
         arg{:});
  end

%% srgb_color_cube.m ends here
