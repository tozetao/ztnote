# Shortcodes

As a security precaution, running PHP inside WordPress content is forbidden; to allow dynamic interactions with the content, Shortcodes were presented in WordPress version 2.5.

为了安全，WP的内容中禁止允许PHP代码。然而为了能够与内容进行动态交互，WP2.5版本中出现了短代码。

Shortcodes are macros that can be used to perform dynamic interactions with the content. i.e creating a gallery from images attached to the post or rendering a video.

段代码是宏，可用于跟内容进行动态交互。例如，利用帖子所附的图片创建图库（gallery）或渲染视频。



## [Why Shortcodes?](https://developer.wordpress.org/plugins/shortcodes/#why-shortcodes)

Shortcodes are a valuable way of keeping content clean and semantic while allowing end users some ability to programmatically alter the presentation of their content.

简码是保持内容简洁和语义的重要方法，同时允许最终用户以编程方式改变内容的表现形式。

When the end user adds a photo gallery to their post using a shortcode, they’re using the least data possible to indicate how the gallery should be presented.

当最终用户使用简码在帖子中添加图片库时，他们会使用尽可能少的数据来显示图片库的显示方式。

Advantages:

- No markup is added to the post content, which means that markup and styling can easily be manipulated on the fly or at a later state.

  无需在帖子内容中添加标记，这意味着标记和样式可以很容易地即时或者在之后进行修改。

- Shortcodes can also accept parameters, allowing users to modify how the shortcode behaves on an instance by instance basis.

  简码还可以接受参数，允许用户逐例修改简码的行为方式。



## [Built-in Shortcodes](https://developer.wordpress.org/plugins/shortcodes/#built-in-shortcodes)

By default, WordPress includes the following shortcodes:

- `[caption]` – allows you to wrap captions around content

- `[gallery]` – allows you to show image galleries

- `[audio]` – allows you to embed and play audio files

  audio - 允许你嵌入和播放音频文件

- `[video]` – allows you to embed and play video files

  video - 允许你嵌入和播放视频文件

- `[playlist]` – allows you to display collection of audio or video files

  可显示音频或视频文件集

- `[embed]` – allows you to wrap embedded items

  允许您包装嵌入式项目



## [Shortcode Best Practices](https://developer.wordpress.org/plugins/shortcodes/#shortcode-best-practices)

Best practices for developing shortcodes include the [plugin development best practices](https://developer.wordpress.org/plugins/the-basics/best-practices/) and the list below:

- **Always return!**
  Shortcodes are essentially filters, so creating “[side effects](https://en.wikipedia.org/wiki/Side_effect_(computer_science))” will lead to unexpected bugs.
- Prefix your shortcode names to avoid collisions with other plugins.
- Sanitize the input and escape the output.
- Provide users with clear documentation on all shortcode attributes.

[Top ↑](https://developer.wordpress.org/plugins/shortcodes/#top)

## [Quick Reference](https://developer.wordpress.org/plugins/shortcodes/#quick-reference)

See the complete example of using a [basic shortcode structure, taking care of self-closing and enclosing scenarios, shortcodes within shortcodes and securing output](https://developer.wordpress.org/plugins/shortcodes/shortcodes-with-parameters/#complete-example).

[Top ↑](https://developer.wordpress.org/plugins/shortcodes/#top)

## [External Resources](https://developer.wordpress.org/plugins/shortcodes/#external-resources)

- [WordPress Shortcodes Generator](http://generatewp.com/shortcodes/)