Posts in "draft" status:

```dataview
TABLE WITHOUT ID title AS "Draft Title", file.link as "Link", dateformat(project-start-date, "yyyy - MMM") AS "Created Date"
WHERE draft AND contains(file.folder, "content")
```
