# > usethis::edit_r_environ()

# EARTHENGINE_INIT_MESSAGE="True"
# EARTHENGINE_PYTHON="/home/vinicio/.virtualenvs/rgee/bin/python"
# EARTHENGINE_ENV="rgee"

library(rgee)


# > ee_Initialize()
# ── rgee 1.1.7 ───────────────────────────────────────────────────────── earthengine-api 0.1.329 ──
# ✔ user: not_defined
# ✔ Initializing Google Earth Engine:  DONE!
#   Error in value[[3L]](cond) :
#   It looks like your EE credential has expired. Try running ee_Authenticate() again or clean your credentials ee_clean_user_credentials().


# > ee_check()
# ◉  Python version
# ✔ [Ok] /usr/bin/python3.8 v3.8
# ◉  Python packages:
#   ✔ [Ok] numpy
# ✔ [Ok] earthengine-api
# NOTE: The Earth Engine Python API version 0.1.329 is installed
# correctly in the system but rgee was tested using the version
# 0.1.370. To avoid possible issues, we recommend install the
# version used by rgee (0.1.370). You might use:
#   * rgee::ee_install_upgrade()
# * reticulate::py_install('earthengine-api==0.1.370', envname='PUT_HERE_YOUR_PYENV')
# * pip install earthengine-api==0.1.370 (Linux and Mac0S)
# * conda install earthengine-api==0.1.370 (Linux, Mac0S, and Windows)
#


# > rgee::ee_check_credentials()
# ◉  Credentials neccesaries for rgee:
#   ✔ [Ok] Earth Engine Credentials found.
# ✔ [Ok] Google Drive credentials found.
# ✔ [Ok] Google Cloud Storage credentials found.
# >

# > rgee::ee_check_python()
# ◉  Python version
# ✔ [Ok] /usr/bin/python3.8 v3.8
# >

ee_path <- path.expand("~/.config/earthengine/credentials")
file.exists(ee_path)

ee_Initialize(user= "viniciovcl@gmail.com", drive = TRUE)
