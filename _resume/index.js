const fs = require('fs')
const path = require('path')
const yaml = require('js-yaml')
const handlebars = require('handlebars')
const resumeYaml = fs.readFileSync(
  path.join(__dirname, 'resume.yaml')
)

function render (resume) {
  const templatePath = path.join(__dirname, 'resume.hbs')
  const template = fs.readFileSync(templatePath, 'utf-8')
  const resumeHtml = handlebars.compile(template)({resume})
  const resumeHtmlPath = path.resolve(__dirname, '../_includes/resume.html')

  fs.writeFileSync(resumeHtmlPath, resumeHtml)
}

render(yaml.safeLoad(resumeYaml))
