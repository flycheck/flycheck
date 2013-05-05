# "top-scope variable being used without an explicit namespace"
class topscope {
  file {'/tmp/somefile1234':
    content = $operatingsystem,
  }
}
