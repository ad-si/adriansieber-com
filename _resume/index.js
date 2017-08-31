var fs = require('fs'),
  path = require('path'),
  yaml = require('js-yaml'),
  handlebars = require('handlebars'),
  resumeYaml = fs.readFileSync(
    path.join(__dirname, 'resume.yaml')
  )

function render (resume) {

  var template = fs.readFileSync(
      path.join(__dirname, 'resume.hbs'),
      'utf-8'
    ),
    resumeHtml = handlebars.compile(template)({
      resume: resume
    })

  fs.writeFileSync(
    path.resolve(__dirname, '../_includes/resume.html'),
    resumeHtml
  )
}

render(yaml.safeLoad(resumeYaml))
