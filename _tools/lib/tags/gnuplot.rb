require 'digest/md5'
require 'erb'

def render_erb(template_source, user_context)
  template = ERB.new(template_source)
  b = binding.clone
  user_context.each do |k, v|
    eval "#{k} = #{v.inspect}", b
  end
  template.result(b)
end


class Gnuplot < Liquid::Block
  def initialize(tag_name, markup, tokens)
    @user_opts = eval(markup) || Hash.new
    super
  end

  def setup(context)
    @global_opts = context.registers[:site].config['gnuplot']['opts'] || Hash.new
    @out_dir = context.registers[:site].config['gnuplot']['out_dir']
    @out_url = context.registers[:site].config['gnuplot']['out_url']
  end

  def file(ext)
    File.join(@out_dir, @md5 + '.' + ext)
  end

  def render(context)
    setup(context)
    content = super.join
    plt, data = content.split("\n---\n", 2)
    @md5 = Digest::MD5.hexdigest(content)

    unless File.exists?(file('png'))
      File.open(file('dat'), "w") do |f|
        f.write(data)
      end

      File.open(file('plt'), "w") do |f|
        context = {
          'png_opts' => "",
          'width' => 400,
          'height' => 400,
          'out_file' => file('png'),
          'data_file' => file('dat'),
        }.merge(@global_opts).merge(@user_opts)
        tmpl_plt = <<EOF
reset
set terminal png transparent enhanced size <%= width %>,<%= height %> <%= png_opts %>
set output '<%= out_file %>'
EOF
        tmpl_plt += plt
        f.write(render_erb(tmpl_plt, context))
      end

      cmd = "gnuplot < #{file('plt')}"
      $stderr.puts cmd
      system cmd
    end
    "\n<div class=\"gnuplot_bitmap\">\n<img src=\"#{@out_url + '/' + @md5 + '.png'}\" alt=\"Gnuplot chart\" />\n</div>\n"
  end
end

Liquid::Template.register_tag('gnuplot', Gnuplot)
