# Taxonomies

A **Taxonomy** is a fancy word for the classification/grouping of things. Taxonomies can be hierarchical (with parents/children) or flat.

Taxonomy是对事物进行分类/分组的一个花哨的词。Taxonomy可以是分层的（有父级/子级），也可以是扁平的。

WordPress stores the Taxonomies in the `term_taxonomy` database table allowing developers to register Custom Taxonomies along the ones that already exist.

WordPress 将Taxonomies 存储在 term_taxonomy 数据库表中，允许开发人员在已有分类的基础上注册自定义Taxonomies 。

Taxonomies have **Terms** which serve as the topic by which you classify/group things. They are stored inside the `terms` table.

Taxonomies有Terms，作为您对事物进行分类/分组的主题。它们存储在terms表中。

For example: a Taxonomy named “Art” can have multiple Terms, such as “Modern” and “18th Century”.

例如一个命名为Art的Taxonomy可以有多个Terms，比如"Modern"和"18th Century".

This chapter will show you how to register Custom Taxonomies, how to retrieve their content from the database, and how to render them to the public.

本章将向你展示如何注册自定义Taxonomies，如何从数据库中检索其内容，以及如何将其呈现给公众。

> Note:WordPress 3.4 and earlier had a Taxonomy named “Links” which was deprecated in WordPress 3.5.
>
> WordPress 3.4 及更早版本有一个名为 "链接 "的分类标准，但在 WordPress 3.5 中已被弃用。