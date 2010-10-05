require 'digest/md5'
require 'tempfile'

class Dot < Liquid::Block
  def initialize(tag_name, markup, tokens)
    @dot_opts = markup
    super
  end

  def render(context)
    code =  super.join
    global_dot_opts = context['dot']['opts']
    out_dir = context['dot']['out_dir']
    out_url = context['dot']['out_url']
    md5 = Digest::MD5.hexdigest(code + global_dot_opts + @dot_opts)
    out_file = File.join(out_dir, md5 + '.png')
    unless File.exists?(out_file)
      # File.join(out_dir, md5 + '.dot')
      dot_file = Tempfile.new('dot').path
      File.open(dot_file, 'w') do |f|
        f.write(code)
      end
      c = "-Tpng -o#{File.join(out_dir, md5 + '.png')}"

      cmd = "dot #{global_dot_opts} #{@dot_opts} #{c} #{dot_file}"
      $stderr.puts cmd
      system cmd
    end
    width, height = IO.read(out_file)[0x10..0x18].unpack('NN') # Png
    "\n<center><div class=\"dot_bitmap\">\n<img src=\"#{out_url + '/' + md5 + '.png'}\" alt=\"Dot graph\" width=\"#{width}\" height=\"#{height}\" />\n</div></center>\n"
  end
end

Liquid::Template.register_tag('dot', Dot)
