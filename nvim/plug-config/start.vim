let g:startify_session_dir = '$XDG_CONFIG_HOME/nvim/sessions'

" starting nvim from a dir containing a `Session.vim` will use that session
" automatically
let g:startify_session_autoload = 1

let g:startify_session_delete_buffers = 1  " startify handles buffers
let g:startify_change_to_vcs_root = 1      " find vcs root, set work dir to that
let g:startify_fortune_use_unicode = 1
let g:startify_session_persistence = 1
let g:startify_enable_special = 0          " do not save empty buffers

let g:startify_lists = [
          \ { 'type': 'files',     'header': ['   Files']            },
          \ { 'type': 'dir',       'header': ['   VCS Dir '. getcwd()] },
          \ { 'type': 'sessions',  'header': ['   Sessions']       },
          \ { 'type': 'bookmarks', 'header': ['   Bookmarks']      },
          \ ]

let g:startify_bookmarks = [
            \ { 'i': '$XDG_CONFIG_HOME/nvim/init.vim' },
            \ ]

" this ASCII font is called `Alligator`
let g:startify_custom_header = [
\'            :::       :::     ::: :::::::::::          :::::::::   :::::::: ',
\'           :+:       :+:   :+: :+:   :+:              :+:    :+: :+:    :+: ',
\'          +:+       +:+  +:+   +:+  +:+              +:+    +:+ +:+    +:+  ',
\'         +#+  +:+  +#+ +#++:++#++: +#+              +#+    +:+ +#+    +:+   ',
\'        +#+ +#+#+ +#+ +#+     +#+ +#+              +#+    +#+ +#+    +#+    ',
\'        #+#+# #+#+#  #+#     #+# #+#              #+#    #+# #+#    #+#     ',
\'        ###   ###   ###     ### ###              #########   ########       ',
\]
