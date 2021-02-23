/*
Navicat MySQL Data Transfer

Source Server         : Local
Source Server Version : 50553
Source Host           : 127.0.0.1:3306
Source Database       : guess

Target Server Type    : MYSQL
Target Server Version : 50553
File Encoding         : 65001

Date: 2020-12-08 16:22:54
*/

SET FOREIGN_KEY_CHECKS=0;

-- ----------------------------
-- Table structure for `account`
-- ----------------------------
DROP TABLE IF EXISTS `account`;
CREATE TABLE `account` (
  `id` int(11) NOT NULL,
  `deposit` bigint(20) NOT NULL DEFAULT '0',
  `balance` bigint(20) NOT NULL DEFAULT '0',
  `withdraw` bigint(20) NOT NULL DEFAULT '0',
  `gift_val` bigint(20) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of account
-- ----------------------------
INSERT INTO `account` VALUES ('1', '0', '0', '0', '0');

-- ----------------------------
-- Table structure for `accounting`
-- ----------------------------
DROP TABLE IF EXISTS `accounting`;
CREATE TABLE `accounting` (
  `id` int(10) unsigned NOT NULL,
  `start_time` int(10) unsigned NOT NULL DEFAULT '0',
  `end_time` int(10) unsigned NOT NULL DEFAULT '0',
  `store` int(10) unsigned NOT NULL DEFAULT '0',
  `doll` int(10) unsigned NOT NULL DEFAULT '0',
  `cami` int(10) unsigned NOT NULL DEFAULT '0',
  `profit_rank` int(10) unsigned NOT NULL DEFAULT '0',
  `cms` int(10) unsigned NOT NULL DEFAULT '0',
  `deposit` int(10) unsigned NOT NULL DEFAULT '0',
  `withdraw` int(10) unsigned NOT NULL DEFAULT '0',
  `time` int(11) unsigned DEFAULT '0' COMMENT '当前时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of accounting
-- ----------------------------
INSERT INTO `accounting` VALUES ('1', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');

-- ----------------------------
-- Table structure for `address`
-- ----------------------------
DROP TABLE IF EXISTS `address`;
CREATE TABLE `address` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `role_id` int(11) NOT NULL,
  `name` text NOT NULL,
  `phone` varchar(40) NOT NULL,
  `address` text NOT NULL,
  `state` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `indexname` (`role_id`,`state`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of address
-- ----------------------------

-- ----------------------------
-- Table structure for `ad_log`
-- ----------------------------
DROP TABLE IF EXISTS `ad_log`;
CREATE TABLE `ad_log` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `room_id` int(10) unsigned NOT NULL COMMENT '0->大厅，其他表示某个房间',
  `content` text NOT NULL COMMENT '广播内容',
  `create_time` int(10) unsigned NOT NULL COMMENT '创建时间',
  `time` int(10) unsigned NOT NULL COMMENT '轮播时间',
  `state` int(10) unsigned NOT NULL COMMENT '广播状态',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of ad_log
-- ----------------------------

-- ----------------------------
-- Table structure for `agent`
-- ----------------------------
DROP TABLE IF EXISTS `agent`;
CREATE TABLE `agent` (
  `id` int(10) unsigned NOT NULL,
  `name` text NOT NULL,
  `parent_id` int(10) unsigned NOT NULL DEFAULT '0',
  `create_time` int(10) unsigned NOT NULL DEFAULT '0',
  `die` int(10) unsigned NOT NULL DEFAULT '0',
  `percent` int(10) unsigned NOT NULL DEFAULT '2000',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of agent
-- ----------------------------

-- ----------------------------
-- Table structure for `agent_account`
-- ----------------------------
DROP TABLE IF EXISTS `agent_account`;
CREATE TABLE `agent_account` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '自增id',
  `agent_id` int(11) NOT NULL DEFAULT '0' COMMENT '代理商ID',
  `deposit` int(11) DEFAULT '0' COMMENT '游戏充值',
  `bs_deposit` int(11) DEFAULT '0' COMMENT '后台充值',
  `profit_rank` int(11) DEFAULT '0' COMMENT '盈利榜红包',
  `cms` int(11) DEFAULT '0' COMMENT '营销红包',
  `doll` int(11) DEFAULT '0' COMMENT '夹娃娃奖品',
  `store` int(11) DEFAULT '0' COMMENT '商城兑换',
  `charge_gift` int(11) DEFAULT '0' COMMENT '充值送分',
  `login_gift` int(11) DEFAULT '0' COMMENT '签到送分',
  `wxpublic_gift` int(11) DEFAULT '0' COMMENT '公众号送分',
  `share_gift` int(11) DEFAULT '0' COMMENT '分享圈送分',
  `bs_gift` int(11) DEFAULT '0' COMMENT '后台送分',
  `profit_rank_gift` int(11) DEFAULT '0' COMMENT '盈利榜送分',
  `cms_gift` int(11) DEFAULT '0' COMMENT '营销送分',
  `daily_task_gift` int(11) DEFAULT '0' COMMENT '每日任务送分',
  `green_gift` int(11) DEFAULT '0' COMMENT '新手送分',
  `zoo` int(11) DEFAULT '0' COMMENT '动物园红包',
  `time` int(11) DEFAULT '0' COMMENT '时间戳',
  PRIMARY KEY (`id`),
  KEY `index_name` (`time`,`agent_id`)
) ENGINE=InnoDB AUTO_INCREMENT=102 DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of agent_account
-- ----------------------------
INSERT INTO `agent_account` VALUES ('1', '40', '0', '1000000', '0', '199740', '0', '0', '0', '98', '0', '0', '0', '0', '15025', '0', '188', '0', '1523548799');
INSERT INTO `agent_account` VALUES ('2', '9', '0', '6000000', '794000', '0', '9000', '5740000', '0', '0', '0', '0', '10000000', '0', '0', '820', '188', '6668000', '1523548799');
INSERT INTO `agent_account` VALUES ('3', '4', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1523548799');
INSERT INTO `agent_account` VALUES ('4', '40', '0', '0', '0', '0', '0', '0', '0', '138', '0', '0', '0', '8000', '0', '0', '0', '354000', '1523894399');
INSERT INTO `agent_account` VALUES ('5', '9', '0', '0', '2000', '0', '0', '0', '0', '138', '0', '0', '0', '2000', '0', '188', '0', '0', '1523894399');
INSERT INTO `agent_account` VALUES ('6', '9', '0', '0', '0', '0', '0', '0', '0', '158', '0', '0', '0', '100000', '0', '188', '0', '92000', '1523980799');
INSERT INTO `agent_account` VALUES ('7', '4', '404940', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '1523980799');
INSERT INTO `agent_account` VALUES ('8', '9', '0', '0', '0', '0', '0', '0', '0', '188', '0', '0', '0', '0', '0', '0', '0', '0', '1524239999');
INSERT INTO `agent_account` VALUES ('9', '40', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1525708799');
INSERT INTO `agent_account` VALUES ('10', '40', '0', '0', '0', '0', '0', '0', '0', '138', '0', '0', '0', '0', '0', '0', '0', '0', '1525795199');
INSERT INTO `agent_account` VALUES ('11', '9', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1527955199');
INSERT INTO `agent_account` VALUES ('12', '15', '0', '0', '0', '0', '0', '0', '0', '138', '0', '0', '0', '0', '0', '0', '0', '0', '1534867199');
INSERT INTO `agent_account` VALUES ('13', '8', '0', '0', '0', '0', '0', '0', '0', '138', '0', '0', '0', '0', '0', '0', '0', '0', '1534867199');
INSERT INTO `agent_account` VALUES ('14', '15', '0', '0', '0', '0', '0', '0', '0', '158', '0', '0', '0', '0', '0', '0', '0', '0', '1534953599');
INSERT INTO `agent_account` VALUES ('15', '8', '0', '0', '0', '0', '0', '0', '0', '158', '0', '0', '0', '0', '0', '0', '0', '0', '1534953599');
INSERT INTO `agent_account` VALUES ('16', '27', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '188', '0', '1534953599');
INSERT INTO `agent_account` VALUES ('17', '15', '0', '0', '0', '0', '0', '0', '0', '188', '0', '0', '0', '0', '0', '0', '0', '0', '1535039999');
INSERT INTO `agent_account` VALUES ('18', '34', '0', '0', '0', '0', '0', '0', '0', '196', '0', '0', '0', '0', '0', '0', '0', '0', '1535039999');
INSERT INTO `agent_account` VALUES ('19', '36', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535039999');
INSERT INTO `agent_account` VALUES ('20', '22', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535039999');
INSERT INTO `agent_account` VALUES ('21', '27', '0', '0', '0', '0', '0', '0', '0', '138', '0', '0', '0', '0', '0', '0', '0', '0', '1535039999');
INSERT INTO `agent_account` VALUES ('22', '27', '0', '0', '0', '0', '0', '0', '0', '158', '0', '0', '0', '0', '0', '68', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('23', '10', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('24', '30', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('25', '22', '0', '0', '0', '0', '0', '0', '0', '138', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('26', '24', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '188', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('27', '38', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('28', '17', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('29', '27', '0', '0', '0', '0', '0', '0', '0', '158', '0', '0', '0', '0', '0', '68', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('30', '10', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('31', '30', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('32', '22', '0', '0', '0', '0', '0', '0', '0', '138', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('33', '24', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '188', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('34', '38', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('35', '17', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('36', '27', '0', '0', '0', '0', '0', '0', '0', '158', '0', '0', '0', '0', '0', '68', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('37', '10', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('38', '30', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('39', '22', '0', '0', '0', '0', '0', '0', '0', '138', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('40', '24', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '188', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('41', '38', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('42', '17', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('43', '27', '0', '0', '0', '0', '0', '0', '0', '158', '0', '0', '0', '0', '0', '68', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('44', '10', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('45', '30', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('46', '22', '0', '0', '0', '0', '0', '0', '0', '138', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('47', '24', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '188', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('48', '38', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('49', '17', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('50', '27', '0', '0', '0', '0', '0', '0', '0', '158', '0', '0', '0', '0', '0', '68', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('51', '10', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('52', '30', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('53', '22', '0', '0', '0', '0', '0', '0', '0', '138', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('54', '24', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '188', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('55', '38', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('56', '17', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('57', '27', '0', '0', '0', '0', '0', '0', '0', '158', '0', '0', '0', '0', '0', '68', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('58', '10', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('59', '30', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('60', '22', '0', '0', '0', '0', '0', '0', '0', '138', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('61', '24', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '188', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('62', '38', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('63', '17', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('64', '27', '0', '0', '0', '0', '0', '0', '0', '158', '0', '0', '0', '0', '0', '68', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('65', '10', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('66', '30', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('67', '22', '0', '0', '0', '0', '0', '0', '0', '138', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('68', '24', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '188', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('69', '38', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('70', '17', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('71', '27', '0', '0', '0', '0', '0', '0', '0', '158', '0', '0', '0', '0', '0', '68', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('72', '10', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('73', '30', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('74', '22', '0', '0', '0', '0', '0', '0', '0', '138', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('75', '24', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '188', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('76', '38', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('77', '17', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1535126399');
INSERT INTO `agent_account` VALUES ('78', '9', '0', '24000', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '1576252799');
INSERT INTO `agent_account` VALUES ('79', '32', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1576511999');
INSERT INTO `agent_account` VALUES ('80', '32', '0', '0', '0', '0', '0', '0', '0', '138', '0', '0', '0', '0', '0', '0', '0', '0', '1576598399');
INSERT INTO `agent_account` VALUES ('81', '32', '0', '0', '0', '0', '0', '0', '0', '158', '0', '0', '0', '0', '0', '0', '0', '0', '1576684799');
INSERT INTO `agent_account` VALUES ('82', '4', '0', '1000000', '0', '0', '0', '0', '0', '98', '0', '0', '5000', '0', '0', '0', '188', '0', '1576684799');
INSERT INTO `agent_account` VALUES ('83', '15', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1577116799');
INSERT INTO `agent_account` VALUES ('84', '32', '0', '0', '0', '0', '0', '0', '0', '188', '0', '0', '0', '8000', '0', '188', '188', '0', '1577116799');
INSERT INTO `agent_account` VALUES ('85', '3', '0', '100000000', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '188', '0', '1577116799');
INSERT INTO `agent_account` VALUES ('86', '15', '0', '0', '0', '0', '0', '0', '0', '138', '0', '0', '0', '0', '0', '0', '0', '0', '1577203199');
INSERT INTO `agent_account` VALUES ('87', '3', '0', '0', '0', '0', '0', '0', '0', '138', '0', '0', '0', '0', '0', '188', '0', '0', '1577203199');
INSERT INTO `agent_account` VALUES ('88', '17', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1577203199');
INSERT INTO `agent_account` VALUES ('89', '15', '0', '0', '0', '0', '0', '0', '0', '158', '0', '0', '0', '0', '0', '0', '0', '0', '1577375999');
INSERT INTO `agent_account` VALUES ('90', '32', '0', '0', '0', '0', '0', '0', '0', '188', '0', '0', '0', '0', '0', '188', '0', '0', '1577375999');
INSERT INTO `agent_account` VALUES ('91', '3', '0', '0', '488000', '0', '0', '0', '0', '158', '0', '0', '0', '0', '0', '376', '0', '0', '1577375999');
INSERT INTO `agent_account` VALUES ('92', '32', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1578067199');
INSERT INTO `agent_account` VALUES ('93', '15', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1578412799');
INSERT INTO `agent_account` VALUES ('94', '32', '0', '0', '0', '0', '0', '0', '0', '138', '0', '0', '0', '0', '0', '0', '0', '0', '1578412799');
INSERT INTO `agent_account` VALUES ('95', '35', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1578412799');
INSERT INTO `agent_account` VALUES ('96', '3', '0', '0', '488000', '0', '0', '0', '0', '138', '0', '0', '0', '0', '0', '188', '0', '0', '1578412799');
INSERT INTO `agent_account` VALUES ('97', '9', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '0', '0', '1578412799');
INSERT INTO `agent_account` VALUES ('98', '17', '0', '0', '0', '0', '0', '0', '0', '98', '0', '0', '0', '0', '0', '0', '188', '0', '1578412799');
INSERT INTO `agent_account` VALUES ('99', '32', '0', '0', '0', '0', '0', '0', '0', '158', '0', '0', '0', '0', '0', '0', '0', '0', '1578499199');
INSERT INTO `agent_account` VALUES ('100', '8', '0', '2000000', '0', '0', '0', '0', '0', '98', '0', '0', '1020000', '0', '0', '0', '0', '0', '1587743999');
INSERT INTO `agent_account` VALUES ('101', '17', '0', '100000', '0', '0', '0', '0', '0', '0', '0', '0', '1000', '0', '0', '188', '0', '0', '1607356799');

-- ----------------------------
-- Table structure for `animal_log`
-- ----------------------------
DROP TABLE IF EXISTS `animal_log`;
CREATE TABLE `animal_log` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `role_id` int(10) unsigned NOT NULL,
  `bet` int(10) unsigned NOT NULL,
  `bet_val` int(10) NOT NULL,
  `win_val` int(10) NOT NULL,
  `time` int(10) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  KEY `animal` (`bet_val`,`win_val`,`role_id`,`time`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=260 DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of animal_log
-- ----------------------------

-- ----------------------------
-- Table structure for `bonus_log`
-- ----------------------------
DROP TABLE IF EXISTS `bonus_log`;
CREATE TABLE `bonus_log` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `role_id` int(20) NOT NULL DEFAULT '0',
  `name` varchar(40) DEFAULT NULL,
  `icon` text,
  `bonus` int(20) NOT NULL DEFAULT '0',
  `time` int(20) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of bonus_log
-- ----------------------------

-- ----------------------------
-- Table structure for `broadcast_log`
-- ----------------------------
DROP TABLE IF EXISTS `broadcast_log`;
CREATE TABLE `broadcast_log` (
  `id` int(6) NOT NULL AUTO_INCREMENT,
  `message` longtext NOT NULL,
  `doll_run` text,
  `mall_run` text,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of broadcast_log
-- ----------------------------
INSERT INTO `broadcast_log` VALUES ('1', 'undefined', '[]', '[]');

-- ----------------------------
-- Table structure for `cami`
-- ----------------------------
DROP TABLE IF EXISTS `cami`;
CREATE TABLE `cami` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `order_id` varchar(40) NOT NULL,
  `reward_id` int(10) unsigned NOT NULL DEFAULT '0',
  `cami` varchar(99) NOT NULL,
  `role_id` int(10) unsigned NOT NULL,
  `val` int(10) unsigned NOT NULL,
  `time` int(10) unsigned NOT NULL,
  `card_no` varchar(99) DEFAULT NULL COMMENT '手机卡号',
  PRIMARY KEY (`id`),
  UNIQUE KEY `order_id` (`order_id`),
  KEY `role_id` (`role_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of cami
-- ----------------------------

-- ----------------------------
-- Table structure for `cat_log`
-- ----------------------------
DROP TABLE IF EXISTS `cat_log`;
CREATE TABLE `cat_log` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `role_id` int(10) unsigned NOT NULL,
  `cat_id` int(10) unsigned NOT NULL,
  `red` int(10) unsigned NOT NULL,
  `blue` int(10) unsigned NOT NULL,
  `result` int(10) unsigned NOT NULL,
  `bet_val` bigint(20) NOT NULL DEFAULT '0',
  `win_val` bigint(20) NOT NULL DEFAULT '0',
  `time` int(10) unsigned NOT NULL,
  `info` varchar(9999) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `time` (`time`,`role_id`,`bet_val`,`win_val`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of cat_log
-- ----------------------------

-- ----------------------------
-- Table structure for `charge_activity`
-- ----------------------------
DROP TABLE IF EXISTS `charge_activity`;
CREATE TABLE `charge_activity` (
  `id` int(10) NOT NULL AUTO_INCREMENT COMMENT '充值活动设置',
  `status` tinyint(4) NOT NULL DEFAULT '0' COMMENT '0未开1开启',
  `gift` int(10) NOT NULL DEFAULT '0' COMMENT '充值赠送钻石',
  `time` int(10) NOT NULL DEFAULT '0' COMMENT '设置充值活动的时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of charge_activity
-- ----------------------------
INSERT INTO `charge_activity` VALUES ('1', '1', '100', '0');

-- ----------------------------
-- Table structure for `cms_log`
-- ----------------------------
DROP TABLE IF EXISTS `cms_log`;
CREATE TABLE `cms_log` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `role_id` int(10) unsigned NOT NULL COMMENT '玩家ID',
  `type` int(10) unsigned NOT NULL COMMENT '1:金豆2:红包',
  `val` int(10) unsigned NOT NULL COMMENT '奖励金额:单位金豆',
  `time` int(10) unsigned NOT NULL,
  `state` int(10) unsigned NOT NULL COMMENT '0;未领取1:已领取',
  PRIMARY KEY (`id`),
  KEY `role_id` (`role_id`),
  KEY `time` (`time`),
  KEY `state` (`state`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of cms_log
-- ----------------------------

-- ----------------------------
-- Table structure for `compensate_log`
-- ----------------------------
DROP TABLE IF EXISTS `compensate_log`;
CREATE TABLE `compensate_log` (
  `id` int(10) NOT NULL AUTO_INCREMENT COMMENT '自增id',
  `role_id` int(10) DEFAULT '0' COMMENT '玩家ID',
  `role_name` varchar(80) DEFAULT '' COMMENT '玩家昵称',
  `master_id` int(10) DEFAULT '0' COMMENT '后台人员ID',
  `master_name` varchar(80) DEFAULT '' COMMENT '后台人员昵称',
  `deposit` int(10) DEFAULT '0' COMMENT '充值金豆',
  `gift_val` int(10) DEFAULT '0' COMMENT '赠送金豆',
  `time` int(11) DEFAULT '0' COMMENT '时间',
  PRIMARY KEY (`id`),
  KEY `index_name` (`role_id`,`master_id`,`time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of compensate_log
-- ----------------------------

-- ----------------------------
-- Table structure for `daily`
-- ----------------------------
DROP TABLE IF EXISTS `daily`;
CREATE TABLE `daily` (
  `room_id` int(10) unsigned NOT NULL,
  `p` bigint(20) NOT NULL DEFAULT '0',
  `w` bigint(20) NOT NULL DEFAULT '0',
  `people` bigint(20) NOT NULL DEFAULT '0',
  `roles` varchar(9999) DEFAULT '[]',
  PRIMARY KEY (`room_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of daily
-- ----------------------------

-- ----------------------------
-- Table structure for `daily_account`
-- ----------------------------
DROP TABLE IF EXISTS `daily_account`;
CREATE TABLE `daily_account` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `time` int(10) unsigned NOT NULL,
  `store` int(10) unsigned NOT NULL,
  `doll` int(10) unsigned NOT NULL,
  `cami` int(10) unsigned NOT NULL,
  `profit_rank` int(10) unsigned NOT NULL,
  `cms` int(10) unsigned NOT NULL,
  `deposit` int(10) unsigned NOT NULL,
  `withdraw` int(10) unsigned NOT NULL,
  `status` int(10) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=96 DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Table structure for `daily_log`
-- ----------------------------
DROP TABLE IF EXISTS `daily_log`;
CREATE TABLE `daily_log` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `room_id` int(10) unsigned NOT NULL,
  `type` int(10) unsigned NOT NULL,
  `p` bigint(20) unsigned NOT NULL,
  `w` bigint(20) unsigned NOT NULL,
  `people` int(10) unsigned NOT NULL,
  `roles` int(10) unsigned NOT NULL,
  `time` int(10) unsigned NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of daily_log
-- ----------------------------

-- ----------------------------
-- Table structure for `dog_run_log`
-- ----------------------------
DROP TABLE IF EXISTS `dog_run_log`;
CREATE TABLE `dog_run_log` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '自增id',
  `type` tinyint(4) DEFAULT '1' COMMENT '1固定赔率，2随机赔率',
  `num` int(11) DEFAULT NULL COMMENT '期数',
  `all_gold` bigint(20) DEFAULT NULL COMMENT '所有押注钱',
  `all_win` bigint(20) DEFAULT NULL COMMENT '所有赔出去的钱',
  `profit` bigint(20) DEFAULT NULL COMMENT '盈利',
  `result` varchar(255) DEFAULT NULL COMMENT '结果列表',
  `role_odds` text COMMENT '玩家押注信息',
  `all_odds` text COMMENT '所有押注赔率',
  `time` int(11) DEFAULT NULL COMMENT '时间戳',
  PRIMARY KEY (`id`),
  KEY `index_name` (`time`,`type`,`profit`,`all_gold`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of dog_run_log
-- ----------------------------

-- ----------------------------
-- Table structure for `dog_run_role`
-- ----------------------------
DROP TABLE IF EXISTS `dog_run_role`;
CREATE TABLE `dog_run_role` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '自增id',
  `role_id` bigint(20) NOT NULL COMMENT '人物id',
  `num` int(11) DEFAULT NULL COMMENT '期数',
  `all_gold` bigint(20) DEFAULT NULL COMMENT '所有押注钱',
  `time` int(11) DEFAULT NULL COMMENT '时间戳',
  `all_win` bigint(20) DEFAULT NULL COMMENT '所有赢的钱',
  `list` text COMMENT '玩家押注信息',
  `all_time` bigint(20) DEFAULT NULL COMMENT '那一期的时间',
  PRIMARY KEY (`id`),
  KEY `index_name` (`role_id`,`all_time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of dog_run_role
-- ----------------------------

-- ----------------------------
-- Table structure for `doll_prize`
-- ----------------------------
DROP TABLE IF EXISTS `doll_prize`;
CREATE TABLE `doll_prize` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `goods_id` int(11) unsigned NOT NULL DEFAULT '0',
  `level` tinyint(2) unsigned NOT NULL DEFAULT '1',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=161 DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of doll_prize
-- ----------------------------
INSERT INTO `doll_prize` VALUES ('16', '40', '2');
INSERT INTO `doll_prize` VALUES ('17', '8', '2');
INSERT INTO `doll_prize` VALUES ('18', '35', '2');
INSERT INTO `doll_prize` VALUES ('22', '7', '3');
INSERT INTO `doll_prize` VALUES ('23', '36', '3');
INSERT INTO `doll_prize` VALUES ('27', '41', '4');
INSERT INTO `doll_prize` VALUES ('28', '37', '4');
INSERT INTO `doll_prize` VALUES ('32', '37', '5');
INSERT INTO `doll_prize` VALUES ('33', '24', '5');
INSERT INTO `doll_prize` VALUES ('34', '38', '5');
INSERT INTO `doll_prize` VALUES ('38', '38', '6');
INSERT INTO `doll_prize` VALUES ('45', '22', '6');
INSERT INTO `doll_prize` VALUES ('46', '8', '0');
INSERT INTO `doll_prize` VALUES ('47', '18', '0');
INSERT INTO `doll_prize` VALUES ('51', '34', '0');
INSERT INTO `doll_prize` VALUES ('64', '47', '2');
INSERT INTO `doll_prize` VALUES ('68', '47', '3');
INSERT INTO `doll_prize` VALUES ('71', '35', '3');
INSERT INTO `doll_prize` VALUES ('72', '22', '4');
INSERT INTO `doll_prize` VALUES ('73', '48', '4');
INSERT INTO `doll_prize` VALUES ('74', '47', '4');
INSERT INTO `doll_prize` VALUES ('75', '36', '4');
INSERT INTO `doll_prize` VALUES ('76', '24', '4');
INSERT INTO `doll_prize` VALUES ('77', '22', '5');
INSERT INTO `doll_prize` VALUES ('79', '23', '5');
INSERT INTO `doll_prize` VALUES ('80', '48', '5');
INSERT INTO `doll_prize` VALUES ('82', '26', '6');
INSERT INTO `doll_prize` VALUES ('83', '23', '6');
INSERT INTO `doll_prize` VALUES ('85', '18', '2');
INSERT INTO `doll_prize` VALUES ('86', '7', '2');
INSERT INTO `doll_prize` VALUES ('87', '41', '3');
INSERT INTO `doll_prize` VALUES ('88', '19', '3');
INSERT INTO `doll_prize` VALUES ('89', '51', '4');
INSERT INTO `doll_prize` VALUES ('90', '51', '5');
INSERT INTO `doll_prize` VALUES ('91', '53', '6');
INSERT INTO `doll_prize` VALUES ('92', '54', '6');
INSERT INTO `doll_prize` VALUES ('93', '52', '6');
INSERT INTO `doll_prize` VALUES ('94', '48', '6');
INSERT INTO `doll_prize` VALUES ('95', '47', '5');
INSERT INTO `doll_prize` VALUES ('98', '41', '2');
INSERT INTO `doll_prize` VALUES ('99', '19', '2');
INSERT INTO `doll_prize` VALUES ('122', '62', '0');
INSERT INTO `doll_prize` VALUES ('123', '27', '1');
INSERT INTO `doll_prize` VALUES ('124', '19', '1');
INSERT INTO `doll_prize` VALUES ('125', '8', '1');
INSERT INTO `doll_prize` VALUES ('126', '62', '1');
INSERT INTO `doll_prize` VALUES ('127', '20', '1');
INSERT INTO `doll_prize` VALUES ('128', '17', '1');
INSERT INTO `doll_prize` VALUES ('129', '18', '1');
INSERT INTO `doll_prize` VALUES ('130', '27', '2');
INSERT INTO `doll_prize` VALUES ('131', '21', '2');
INSERT INTO `doll_prize` VALUES ('132', '4', '2');
INSERT INTO `doll_prize` VALUES ('134', '28', '3');
INSERT INTO `doll_prize` VALUES ('146', '30', '6');
INSERT INTO `doll_prize` VALUES ('147', '25', '6');
INSERT INTO `doll_prize` VALUES ('148', '28', '4');
INSERT INTO `doll_prize` VALUES ('149', '4', '4');
INSERT INTO `doll_prize` VALUES ('150', '21', '3');
INSERT INTO `doll_prize` VALUES ('151', '36', '0');
INSERT INTO `doll_prize` VALUES ('152', '35', '0');
INSERT INTO `doll_prize` VALUES ('153', '19', '0');
INSERT INTO `doll_prize` VALUES ('154', '7', '0');
INSERT INTO `doll_prize` VALUES ('155', '28', '5');
INSERT INTO `doll_prize` VALUES ('156', '25', '5');
INSERT INTO `doll_prize` VALUES ('157', '4', '5');
INSERT INTO `doll_prize` VALUES ('158', '66', '0');
INSERT INTO `doll_prize` VALUES ('159', '66', '1');
INSERT INTO `doll_prize` VALUES ('160', '1', '6');

-- ----------------------------
-- Table structure for `error_message`
-- ----------------------------
DROP TABLE IF EXISTS `error_message`;
CREATE TABLE `error_message` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `role_id` int(11) unsigned NOT NULL,
  `name` text NOT NULL,
  `time` int(11) unsigned NOT NULL,
  `error` text NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of error_message
-- ----------------------------

-- ----------------------------
-- Table structure for `fish_log`
-- ----------------------------
DROP TABLE IF EXISTS `fish_log`;
CREATE TABLE `fish_log` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `role_id` int(10) unsigned NOT NULL,
  `fish` text NOT NULL,
  `bet_val` int(10) unsigned NOT NULL,
  `win` int(10) unsigned NOT NULL,
  `time` int(10) unsigned NOT NULL,
  `type` int(10) unsigned NOT NULL DEFAULT '1' COMMENT '1钓鱼2深海宝藏',
  PRIMARY KEY (`id`),
  KEY `time` (`time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of fish_log
-- ----------------------------

-- ----------------------------
-- Table structure for `fruit_log`
-- ----------------------------
DROP TABLE IF EXISTS `fruit_log`;
CREATE TABLE `fruit_log` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '自增id',
  `role_id` int(11) NOT NULL DEFAULT '0' COMMENT '人物id',
  `num` int(11) NOT NULL DEFAULT '0' COMMENT '期数',
  `bet_val` bigint(20) DEFAULT NULL COMMENT '所有押注钱',
  `win_val` bigint(20) DEFAULT NULL COMMENT '所有赔出去的钱',
  `profit` bigint(20) DEFAULT NULL COMMENT '盈亏',
  `bets` text COMMENT '下注信息',
  `time` int(11) DEFAULT NULL COMMENT '时间戳',
  PRIMARY KEY (`id`),
  KEY `index_name` (`time`,`role_id`,`num`,`bet_val`,`win_val`,`profit`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of fruit_log
-- ----------------------------

-- ----------------------------
-- Table structure for `fruit_result`
-- ----------------------------
DROP TABLE IF EXISTS `fruit_result`;
CREATE TABLE `fruit_result` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '自增id',
  `num` int(11) NOT NULL DEFAULT '0' COMMENT '期数',
  `time` int(11) DEFAULT '0' COMMENT '时间戳',
  `fruit` text COMMENT '开奖水果',
  `bs` varchar(255) DEFAULT NULL COMMENT '开奖大小',
  `type` tinyint(4) DEFAULT NULL COMMENT '开奖类型',
  `odd_light` tinyint(4) DEFAULT NULL COMMENT '倍率灯',
  `wolf_pos` tinyint(4) DEFAULT NULL COMMENT '狼灯位置',
  `bet_val` bigint(20) DEFAULT NULL COMMENT '输入的钱',
  `win_val` bigint(20) DEFAULT NULL COMMENT '输出的钱',
  `profit` bigint(20) DEFAULT NULL COMMENT '盈亏',
  `people` int(11) DEFAULT NULL COMMENT '玩的人数',
  PRIMARY KEY (`id`),
  KEY `index_name` (`time`,`num`,`bet_val`,`win_val`,`profit`)
) ENGINE=InnoDB AUTO_INCREMENT=139120 DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of fruit_result
-- ----------------------------

-- ----------------------------
-- Table structure for `goods`
-- ----------------------------
DROP TABLE IF EXISTS `goods`;
CREATE TABLE `goods` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(40) NOT NULL DEFAULT '',
  `type` tinyint(2) unsigned NOT NULL DEFAULT '1',
  `val` int(10) unsigned NOT NULL DEFAULT '1',
  `rmb` int(10) unsigned NOT NULL DEFAULT '0',
  `picture` text COMMENT '奖品图片',
  `prize_id` int(11) NOT NULL DEFAULT '0' COMMENT '奖品ID',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=68 DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of goods
-- ----------------------------
INSERT INTO `goods` VALUES ('2', '588元红包', '4', '588000', '588', '/company_api/public/goods/prize_red_bag.png', '4');
INSERT INTO `goods` VALUES ('3', '288元红包', '4', '288000', '288', '/company_api/public/goods/prize_red_bag.png', '4');
INSERT INTO `goods` VALUES ('4', '10元红包', '4', '10000', '10', '/company_api/public/goods/prize_red_bag.png', '4');
INSERT INTO `goods` VALUES ('7', '1888金豆', '1', '1888', '0', '/company_api/public/goods/prize_icon_coins.png', '1');
INSERT INTO `goods` VALUES ('8', '888金豆', '1', '888', '0', '/company_api/public/goods/prize_icon_coins.png', '1');
INSERT INTO `goods` VALUES ('9', '惠普cp1200打印机', '5', '1200000', '1000', '/company_api/public/goods/prize_printer.png', '5');
INSERT INTO `goods` VALUES ('10', 'MacBook13寸', '5', '8400000', '6988', '/company_api/public/goods/prize_macbook.png', '7');
INSERT INTO `goods` VALUES ('11', 'ipad mini4(颜色随机)', '5', '3500000', '2888', '/company_api/public/goods/prize_ipad.png', '6');
INSERT INTO `goods` VALUES ('12', '魔声耳机2.0(颜色随机)', '5', '2800000', '2399', '/company_api/public/goods/prize_earpot.png', '11');
INSERT INTO `goods` VALUES ('13', 'iphone7(颜色随机)', '5', '6500000', '5388', '/company_api/public/goods/prize_iphone7.png', '8');
INSERT INTO `goods` VALUES ('14', '中国黄金5g', '5', '1800000', '1500', '/company_api/public/goods/prize_china_gold.png', '10');
INSERT INTO `goods` VALUES ('15', '卡西欧手表(颜色随机)', '5', '1100000', '900', '/company_api/public/goods/prize_watch.png', '9');
INSERT INTO `goods` VALUES ('17', '288金豆', '1', '288', '0', '/company_api/public/goods/prize_icon_coins.png', '1');
INSERT INTO `goods` VALUES ('18', '588金豆', '1', '588', '0', '/company_api/public/goods/prize_icon_coins.png', '1');
INSERT INTO `goods` VALUES ('19', '1288金豆', '1', '1288', '0', '/company_api/public/goods/prize_icon_coins.png', '1');
INSERT INTO `goods` VALUES ('20', '2元红包', '4', '2000', '2', '/company_api/public/goods/prize_red_bag.png', '4');
INSERT INTO `goods` VALUES ('21', '5元红包', '4', '5000', '5', '/company_api/public/goods/prize_red_bag.png', '4');
INSERT INTO `goods` VALUES ('22', '50元话费', '2', '60000', '50', '/company_api/public/goods/prize_phone_card.png', '3');
INSERT INTO `goods` VALUES ('23', '100元话费', '2', '120000', '100', '/company_api/public/goods/prize_phone_card.png', '3');
INSERT INTO `goods` VALUES ('24', '8888金豆', '1', '8888', '0', '/company_api/public/goods/prize_icon_coins.png', '1');
INSERT INTO `goods` VALUES ('25', '15元红包', '4', '15000', '15', '/company_api/public/goods/prize_red_bag.png', '4');
INSERT INTO `goods` VALUES ('26', '200元话费', '2', '240000', '200', '/company_api/public/goods/prize_phone_card.png', '3');
INSERT INTO `goods` VALUES ('27', '3元红包', '4', '3000', '3', '/company_api/public/goods/prize_red_bag.png', '4');
INSERT INTO `goods` VALUES ('28', '8元红包', '4', '8000', '8', '/company_api/public/goods/prize_red_bag.png', '4');
INSERT INTO `goods` VALUES ('29', '18元红包', '4', '18000', '18', '/company_api/public/goods/prize_red_bag.png', '4');
INSERT INTO `goods` VALUES ('30', '25元红包', '4', '25000', '25', '/company_api/public/goods/prize_red_bag.png', '4');
INSERT INTO `goods` VALUES ('31', '50元红包', '4', '50000', '50', '/company_api/public/goods/prize_red_bag.png', '4');
INSERT INTO `goods` VALUES ('32', '100元红包', '4', '100000', '100', '/company_api/public/goods/prize_red_bag.png', '4');
INSERT INTO `goods` VALUES ('33', '188金豆', '1', '188', '0', '/company_api/public/goods/prize_icon_coins.png', '1');
INSERT INTO `goods` VALUES ('34', '388金豆', '1', '388', '0', '/company_api/public/goods/prize_icon_coins.png', '1');
INSERT INTO `goods` VALUES ('35', '1688金豆', '1', '1688', '0', '/company_api/public/goods/prize_icon_coins.png', '1');
INSERT INTO `goods` VALUES ('36', '3888金豆', '1', '3888', '0', '/company_api/public/goods/prize_icon_coins.png', '1');
INSERT INTO `goods` VALUES ('37', '6888金豆', '1', '6888', '0', '/company_api/public/goods/prize_icon_coins.png', '1');
INSERT INTO `goods` VALUES ('38', '12888金豆', '1', '12888', '0', '/company_api/public/goods/prize_icon_coins.png', '1');
INSERT INTO `goods` VALUES ('39', '1元红包', '4', '1000', '1', '/company_api/public/goods/prize_red_bag.png', '4');
INSERT INTO `goods` VALUES ('40', '688金豆', '1', '688', '0', '/company_api/public/goods/prize_icon_coins.png', '1');
INSERT INTO `goods` VALUES ('41', '2888金豆', '1', '2888', '0', '/company_api/public/goods/prize_icon_coins.png', '1');
INSERT INTO `goods` VALUES ('42', '15元红包', '4', '15000', '15', '/company_api/public/goods/prize_red_bag.png', '4');
INSERT INTO `goods` VALUES ('43', '50元京东卡', '3', '60000', '50', '/company_api/public/goods/prize_jd_card.png', '2');
INSERT INTO `goods` VALUES ('44', '100元京东卡', '3', '120000', '100', '/company_api/public/goods/prize_jd_card.png', '2');
INSERT INTO `goods` VALUES ('45', '200元京东卡', '3', '240000', '200', '/company_api/public/goods/prize_jd_card.png', '2');
INSERT INTO `goods` VALUES ('47', '20元话费', '2', '24000', '20', '/company_api/public/goods/prize_phone_card.png', '3');
INSERT INTO `goods` VALUES ('48', '30元话费', '2', '36000', '30', '/company_api/public/goods/prize_phone_card.png', '3');
INSERT INTO `goods` VALUES ('51', '5288金豆', '1', '5288', '0', '/company_api/public/goods/prize_icon_coins.png', '1');
INSERT INTO `goods` VALUES ('52', '16888金豆', '1', '16888', '0', '/company_api/public/goods/prize_icon_coins.png', '1');
INSERT INTO `goods` VALUES ('53', '23888金豆', '1', '23888', '0', '/company_api/public/goods/prize_icon_coins.png', '1');
INSERT INTO `goods` VALUES ('54', '52888金豆', '1', '52888', '0', '/company_api/public/goods/prize_icon_coins.png', '1');
INSERT INTO `goods` VALUES ('55', '500m流量', '6', '43200', '36', '/company_api/public/goods/prize_gprs.png', '12');
INSERT INTO `goods` VALUES ('56', '10奖券', '8', '1000', '1', '/company_api/public/goods/prize_chip.png', '14');
INSERT INTO `goods` VALUES ('57', '20奖券', '8', '2000', '2', '/company_api/public/goods/prize_chip.png', '14');
INSERT INTO `goods` VALUES ('58', '50奖券', '8', '5000', '5', '/company_api/public/goods/prize_chip.png', '14');
INSERT INTO `goods` VALUES ('59', '100奖券', '8', '10000', '10', '/company_api/public/goods/prize_chip.png', '14');
INSERT INTO `goods` VALUES ('60', '180奖券', '8', '18000', '18', '/company_api/public/goods/prize_chip.png', '14');
INSERT INTO `goods` VALUES ('61', '250奖券', '8', '25000', '25', '/company_api/public/goods/prize_chip.png', '14');
INSERT INTO `goods` VALUES ('62', '1元红包', '4', '1000', '1', '/company_api/public/goods/prize_red_bag.png', '4');
INSERT INTO `goods` VALUES ('63', '2元红包\n', '4', '2000', '2', '/company_api/public/goods/prize_red_bag.png', '4');
INSERT INTO `goods` VALUES ('64', '200元红包', '4', '240000', '200', '/company_api/public/goods/prize_red_bag.png', '4');
INSERT INTO `goods` VALUES ('65', '1000奖券', '8', '117600', '98', 'resource/prize_res/prize_phone_card.png', '13');
INSERT INTO `goods` VALUES ('66', '10奖券', '8', '1200', '1', 'resource/prize_res/prize_chip.png', '14');
INSERT INTO `goods` VALUES ('67', '14块电池', '3', '16800', '14', 'resource/prize_res/prize_jd_card.png', '2');

-- ----------------------------
-- Table structure for `guess`
-- ----------------------------
DROP TABLE IF EXISTS `guess`;
CREATE TABLE `guess` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `state` int(11) NOT NULL DEFAULT '1' COMMENT '竞猜状态1->竞猜中2->结束竞猜3->关闭竞猜4->未开始',
  `title` text NOT NULL COMMENT '题目',
  `content` text NOT NULL,
  `type` int(11) NOT NULL DEFAULT '1' COMMENT '竞猜类型',
  `select` varchar(999) NOT NULL,
  `time` int(11) NOT NULL,
  `start_time` int(11) NOT NULL DEFAULT '0' COMMENT '开始时间',
  `end_time` int(11) NOT NULL DEFAULT '0' COMMENT '结束时间',
  `draw_time` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '结算时间',
  `picture` text,
  `people` varchar(999) NOT NULL DEFAULT '[]' COMMENT '竞猜过该人',
  `result` varchar(11) DEFAULT NULL,
  `p` int(11) NOT NULL DEFAULT '0',
  `w` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of guess
-- ----------------------------

-- ----------------------------
-- Table structure for `guess_log`
-- ----------------------------
DROP TABLE IF EXISTS `guess_log`;
CREATE TABLE `guess_log` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `guess_id` int(10) unsigned NOT NULL,
  `select` varchar(999) NOT NULL,
  `role_id` int(10) unsigned NOT NULL,
  `name` text,
  `time` int(10) unsigned NOT NULL,
  `bet` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '玩家竞猜选项',
  `val` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '下注金额',
  `odds` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '当时的赔率',
  `bets` varchar(99) NOT NULL DEFAULT '[]',
  `result` int(11) unsigned DEFAULT NULL,
  `win` int(10) unsigned NOT NULL DEFAULT '0',
  `state` int(10) unsigned NOT NULL COMMENT '0->未发奖1->已发奖',
  `end_time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '开奖时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of guess_log
-- ----------------------------

-- ----------------------------
-- Table structure for `init_goods`
-- ----------------------------
DROP TABLE IF EXISTS `init_goods`;
CREATE TABLE `init_goods` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(40) NOT NULL DEFAULT '',
  `type` tinyint(2) unsigned NOT NULL DEFAULT '1',
  `val` int(10) unsigned NOT NULL DEFAULT '1',
  `rmb` int(10) unsigned NOT NULL DEFAULT '0',
  `picture` text COMMENT '奖品图片',
  `prize_id` int(11) NOT NULL DEFAULT '0' COMMENT '奖品ID',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=62 DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of init_goods
-- ----------------------------
INSERT INTO `init_goods` VALUES ('1', '1000元红包', '4', '1000000', '1000', 'resource/prize_res/prize_red_bag.png', '4');
INSERT INTO `init_goods` VALUES ('2', '588元红包', '4', '588000', '588', 'resource/prize_res/prize_red_bag.png', '4');
INSERT INTO `init_goods` VALUES ('3', '288元红包', '4', '288000', '288', 'resource/prize_res/prize_red_bag.png', '4');
INSERT INTO `init_goods` VALUES ('4', '10元红包', '4', '10800', '9', 'resource/prize_res/prize_red_bag.png', '4');
INSERT INTO `init_goods` VALUES ('7', '1888金豆', '1', '1888', '0', 'resource/prize_res/prize_icon_coins.png', '1');
INSERT INTO `init_goods` VALUES ('8', '888金豆', '1', '888', '0', 'resource/prize_res/prize_icon_coins.png', '1');
INSERT INTO `init_goods` VALUES ('9', '惠普cp1200打印机', '5', '1200000', '1000', 'resource/prize_res/prize_printer.png', '5');
INSERT INTO `init_goods` VALUES ('10', 'MacBook13寸', '5', '8400000', '6988', 'resource/prize_res/prize_macbook.png', '7');
INSERT INTO `init_goods` VALUES ('11', 'ipad mini4(颜色随机)', '5', '3500000', '2888', 'resource/prize_res/prize_ipad.png', '6');
INSERT INTO `init_goods` VALUES ('12', '魔声耳机2.0(颜色随机)', '5', '2800000', '2399', 'resource/prize_res/prize_earpot.png', '11');
INSERT INTO `init_goods` VALUES ('13', 'iphone7(颜色随机)', '5', '6600000', '5500', 'resource/prize_res/prize_iphone7.png', '8');
INSERT INTO `init_goods` VALUES ('14', '中国黄金5g', '5', '1800000', '1500', 'resource/prize_res/prize_china_gold.png', '10');
INSERT INTO `init_goods` VALUES ('15', '卡西欧手表(颜色随机)', '5', '1100000', '900', 'resource/prize_res/prize_watch.png', '9');
INSERT INTO `init_goods` VALUES ('16', 'iphone7 plus(颜色随机)', '5', '7700000', '6388', 'resource/prize_res/prize_iphone7.png', '8');
INSERT INTO `init_goods` VALUES ('17', '288金豆', '1', '288', '0', 'resource/prize_res/prize_icon_coins.png', '1');
INSERT INTO `init_goods` VALUES ('18', '588金豆', '1', '588', '0', 'resource/prize_res/prize_icon_coins.png', '1');
INSERT INTO `init_goods` VALUES ('19', '1288金豆', '1', '1288', '0', 'resource/prize_res/prize_icon_coins.png', '1');
INSERT INTO `init_goods` VALUES ('20', '2元红包', '4', '2000', '2', 'resource/prize_res/prize_red_bag.png', '4');
INSERT INTO `init_goods` VALUES ('21', '5元红包', '4', '5000', '5', 'resource/prize_res/prize_red_bag.png', '4');
INSERT INTO `init_goods` VALUES ('22', '50元话费', '2', '60000', '50', 'resource/prize_res/prize_phone_card.png', '3');
INSERT INTO `init_goods` VALUES ('23', '100元话费', '2', '120000', '100', 'resource/prize_res/prize_phone_card.png', '3');
INSERT INTO `init_goods` VALUES ('24', '8888金豆', '1', '8888', '0', 'resource/prize_res/prize_icon_coins.png', '1');
INSERT INTO `init_goods` VALUES ('25', '15元红包', '4', '15000', '15', 'resource/prize_res/prize_red_bag.png', '4');
INSERT INTO `init_goods` VALUES ('26', '200元话费', '2', '240000', '200', 'resource/prize_res/prize_phone_card.png', '3');
INSERT INTO `init_goods` VALUES ('27', '3元红包', '4', '3000', '3', 'resource/prize_res/prize_red_bag.png', '4');
INSERT INTO `init_goods` VALUES ('28', '8元红包', '4', '8000', '8', 'resource/prize_res/prize_red_bag.png', '4');
INSERT INTO `init_goods` VALUES ('29', '18元红包', '4', '18000', '18', 'resource/prize_res/prize_red_bag.png', '4');
INSERT INTO `init_goods` VALUES ('30', '25元红包', '4', '25000', '25', 'resource/prize_res/prize_red_bag.png', '4');
INSERT INTO `init_goods` VALUES ('31', '50元红包', '4', '50000', '50', 'resource/prize_res/prize_red_bag.png', '4');
INSERT INTO `init_goods` VALUES ('32', '100元红包', '4', '100000', '100', 'resource/prize_res/prize_red_bag.png', '4');
INSERT INTO `init_goods` VALUES ('33', '188金豆', '1', '188', '0', 'resource/prize_res/prize_icon_coins.png', '1');
INSERT INTO `init_goods` VALUES ('34', '388金豆', '1', '388', '0', 'resource/prize_res/prize_icon_coins.png', '1');
INSERT INTO `init_goods` VALUES ('35', '1688金豆', '1', '1688', '0', 'resource/prize_res/prize_icon_coins.png', '1');
INSERT INTO `init_goods` VALUES ('36', '3888金豆', '1', '3888', '0', 'resource/prize_res/prize_icon_coins.png', '1');
INSERT INTO `init_goods` VALUES ('37', '6888金豆', '1', '6888', '0', 'resource/prize_res/prize_icon_coins.png', '1');
INSERT INTO `init_goods` VALUES ('38', '12888金豆', '1', '12888', '0', 'resource/prize_res/prize_icon_coins.png', '1');
INSERT INTO `init_goods` VALUES ('39', '1元红包', '4', '1000', '1', 'resource/prize_res/prize_red_bag.png', '4');
INSERT INTO `init_goods` VALUES ('40', '688金豆', '1', '688', '0', 'resource/prize_res/prize_icon_coins.png', '1');
INSERT INTO `init_goods` VALUES ('41', '2888金豆', '1', '2888', '0', 'resource/prize_res/prize_icon_coins.png', '1');
INSERT INTO `init_goods` VALUES ('42', '15元红包', '4', '15000', '15', 'resource/prize_res/prize_red_bag.png', '4');
INSERT INTO `init_goods` VALUES ('43', '50元京东卡', '3', '60000', '50', 'resource/prize_res/prize_jd_card.png', '2');
INSERT INTO `init_goods` VALUES ('44', '100元京东卡', '3', '120000', '100', 'resource/prize_res/prize_jd_card.png', '2');
INSERT INTO `init_goods` VALUES ('45', '200元京东卡', '3', '240000', '200', 'resource/prize_res/prize_jd_card.png', '2');
INSERT INTO `init_goods` VALUES ('47', '20元话费', '2', '24000', '20', 'resource/prize_res/prize_phone_card.png', '3');
INSERT INTO `init_goods` VALUES ('48', '30元话费', '2', '36000', '30', 'resource/prize_res/prize_phone_card.png', '3');
INSERT INTO `init_goods` VALUES ('51', '5288金豆', '1', '5288', '0', 'resource/prize_res/prize_icon_coins.png', '1');
INSERT INTO `init_goods` VALUES ('52', '16888金豆', '1', '16888', '0', 'resource/prize_res/prize_icon_coins.png', '1');
INSERT INTO `init_goods` VALUES ('53', '23888金豆', '1', '23888', '0', 'resource/prize_res/prize_icon_coins.png', '1');
INSERT INTO `init_goods` VALUES ('54', '52888金豆', '1', '52888', '0', 'resource/prize_res/prize_icon_coins.png', '1');
INSERT INTO `init_goods` VALUES ('55', '500M流量', '6', '43200', '36', 'resource/prize_res/prize_gprs.png', '12');
INSERT INTO `init_goods` VALUES ('56', '10奖券', '8', '1000', '1', 'resource/prize_res/prize_chip.png', '14');
INSERT INTO `init_goods` VALUES ('57', '20奖券', '8', '2000', '2', 'resource/prize_res/prize_chip.png', '14');
INSERT INTO `init_goods` VALUES ('58', '50奖券', '8', '5000', '5', 'resource/prize_res/prize_chip.png', '14');
INSERT INTO `init_goods` VALUES ('59', '100奖券', '8', '10000', '10', 'resource/prize_res/prize_chip.png', '14');
INSERT INTO `init_goods` VALUES ('60', '180奖券', '8', '18000', '18', 'resource/prize_res/prize_chip.png', '14');
INSERT INTO `init_goods` VALUES ('61', '250奖券', '8', '25000', '25', 'resource/prize_res/prize_chip.png', '14');

-- ----------------------------
-- Table structure for `jdc_list`
-- ----------------------------
DROP TABLE IF EXISTS `jdc_list`;
CREATE TABLE `jdc_list` (
  `id` int(10) NOT NULL AUTO_INCREMENT COMMENT '自增id',
  `val` int(10) DEFAULT '0',
  `cami` varchar(40) DEFAULT '',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of jdc_list
-- ----------------------------

-- ----------------------------
-- Table structure for `logintime_log`
-- ----------------------------
DROP TABLE IF EXISTS `logintime_log`;
CREATE TABLE `logintime_log` (
  `role_id` int(11) NOT NULL DEFAULT '0',
  `day1` int(10) NOT NULL DEFAULT '0' COMMENT '第一天的登录时间',
  `day2` int(10) NOT NULL DEFAULT '0' COMMENT '第二天的登录时间',
  `day3` int(10) NOT NULL DEFAULT '0' COMMENT '第三天的登录时间',
  `day4` int(10) NOT NULL DEFAULT '0' COMMENT '第四天的登录时间',
  `day5` int(10) NOT NULL DEFAULT '0' COMMENT '第五天的登录时间',
  `day6` int(10) NOT NULL DEFAULT '0' COMMENT '第六天的登录时间',
  `day7` int(10) NOT NULL DEFAULT '0' COMMENT '第七天的登录时间',
  PRIMARY KEY (`role_id`),
  KEY `role_id` (`role_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of logintime_log
-- ----------------------------

-- ----------------------------
-- Table structure for `log_account`
-- ----------------------------
DROP TABLE IF EXISTS `log_account`;
CREATE TABLE `log_account` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `start_time` int(11) unsigned NOT NULL,
  `end_time` int(11) unsigned NOT NULL,
  `deposit` bigint(20) unsigned NOT NULL,
  `withdraw` bigint(20) unsigned NOT NULL,
  `agent_val` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT '提成',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of log_account
-- ----------------------------

-- ----------------------------
-- Table structure for `log_act_game_player`
-- ----------------------------
DROP TABLE IF EXISTS `log_act_game_player`;
CREATE TABLE `log_act_game_player` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `farm` int(11) NOT NULL DEFAULT '0',
  `good_luck` int(11) NOT NULL DEFAULT '0',
  `fish` int(11) NOT NULL DEFAULT '0',
  `stock` int(11) NOT NULL DEFAULT '0',
  `zoo` int(11) NOT NULL DEFAULT '0',
  `dog` int(11) NOT NULL DEFAULT '0',
  `cat` int(11) NOT NULL DEFAULT '0',
  `rps` int(11) NOT NULL DEFAULT '0',
  `guess` int(11) NOT NULL DEFAULT '0',
  `doll` int(11) NOT NULL DEFAULT '0',
  `time` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of log_act_game_player
-- ----------------------------

-- ----------------------------
-- Table structure for `log_chip`
-- ----------------------------
DROP TABLE IF EXISTS `log_chip`;
CREATE TABLE `log_chip` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `role_id` int(10) unsigned NOT NULL,
  `time` int(10) unsigned NOT NULL,
  `amount` int(10) NOT NULL,
  `chip` int(10) unsigned NOT NULL,
  `type` int(10) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  KEY `type` (`type`),
  KEY `time` (`time`),
  KEY `role_id` (`role_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of log_chip
-- ----------------------------

-- ----------------------------
-- Table structure for `log_feedback`
-- ----------------------------
DROP TABLE IF EXISTS `log_feedback`;
CREATE TABLE `log_feedback` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `role_id` int(10) unsigned NOT NULL,
  `nickname` text,
  `icon` text,
  `content` longtext NOT NULL COMMENT '反馈内容',
  `time` bigint(20) unsigned NOT NULL,
  `state` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '是否阅读',
  `reply` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '回复的人',
  PRIMARY KEY (`id`),
  KEY `index_name` (`role_id`,`time`,`state`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of log_feedback
-- ----------------------------
INSERT INTO `log_feedback` VALUES ('1', '20048', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1607335131660904', '0', '0');

-- ----------------------------
-- Table structure for `log_master`
-- ----------------------------
DROP TABLE IF EXISTS `log_master`;
CREATE TABLE `log_master` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `role_id` int(10) unsigned NOT NULL,
  `master_id` int(10) unsigned NOT NULL,
  `name` text,
  `type` int(10) unsigned NOT NULL,
  `val` bigint(20) DEFAULT '0',
  `ip` varchar(40) DEFAULT NULL,
  `time` int(10) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  KEY `master_id` (`master_id`),
  KEY `type` (`type`),
  KEY `time` (`time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of log_master
-- ----------------------------

-- ----------------------------
-- Table structure for `log_online`
-- ----------------------------
DROP TABLE IF EXISTS `log_online`;
CREATE TABLE `log_online` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `info` text NOT NULL,
  `time` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of log_online
-- ----------------------------

-- ----------------------------
-- Table structure for `mall_prize`
-- ----------------------------
DROP TABLE IF EXISTS `mall_prize`;
CREATE TABLE `mall_prize` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `goods_id` int(10) unsigned NOT NULL DEFAULT '0',
  `num` int(11) NOT NULL DEFAULT '5' COMMENT '商品剩余数量',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=10 DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of mall_prize
-- ----------------------------
INSERT INTO `mall_prize` VALUES ('1', '9', '5');
INSERT INTO `mall_prize` VALUES ('2', '10', '5');
INSERT INTO `mall_prize` VALUES ('3', '11', '5');
INSERT INTO `mall_prize` VALUES ('4', '12', '5');
INSERT INTO `mall_prize` VALUES ('5', '13', '5');
INSERT INTO `mall_prize` VALUES ('6', '14', '5');
INSERT INTO `mall_prize` VALUES ('7', '15', '5');
INSERT INTO `mall_prize` VALUES ('8', '16', '5');
INSERT INTO `mall_prize` VALUES ('9', '67', '5');

-- ----------------------------
-- Table structure for `master`
-- ----------------------------
DROP TABLE IF EXISTS `master`;
CREATE TABLE `master` (
  `master_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `parent_id` int(10) unsigned NOT NULL DEFAULT '0',
  `level` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '级别0:zc1:代理商2:其他',
  `agent_id` int(10) unsigned NOT NULL DEFAULT '0',
  `permission` longtext,
  `name` varchar(40) DEFAULT NULL,
  `create_time` int(10) NOT NULL DEFAULT '0',
  `account` varchar(40) NOT NULL DEFAULT '',
  `password` varchar(40) NOT NULL DEFAULT '',
  `disable` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '0启用/1禁用',
  `die` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '0:正常,1:删除',
  `last_login` int(10) unsigned NOT NULL DEFAULT '0',
  `agent_name` text,
  `online` int(11) DEFAULT '0',
  PRIMARY KEY (`master_id`)
) ENGINE=InnoDB AUTO_INCREMENT=12 DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of master
-- ----------------------------
INSERT INTO `master` VALUES ('6', '0', '1', '0', '[1,2,3,4,5,6,7,8,9,10,11,12,13,14]', 'supersuper', '1521792923', 'supersuper', '123456', '0', '0', '1607332710', null, '0');

-- ----------------------------
-- Table structure for `mission`
-- ----------------------------
DROP TABLE IF EXISTS `mission`;
CREATE TABLE `mission` (
  `id` int(11) NOT NULL,
  `lv1` int(11) NOT NULL,
  `lv2` int(11) NOT NULL,
  `lv3` int(11) NOT NULL,
  `lv4` int(11) NOT NULL,
  `lv5` int(11) NOT NULL,
  `lv6` int(11) NOT NULL,
  `lv7` int(11) NOT NULL,
  `lv8` int(11) NOT NULL,
  `lv9` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of mission
-- ----------------------------
INSERT INTO `mission` VALUES ('1', '98', '138', '158', '188', '188', '228', '258', '200', '300');

-- ----------------------------
-- Table structure for `onlines_log`
-- ----------------------------
DROP TABLE IF EXISTS `onlines_log`;
CREATE TABLE `onlines_log` (
  `time` int(10) NOT NULL DEFAULT '0' COMMENT '记录时间戳',
  `2` int(10) NOT NULL DEFAULT '0' COMMENT '2时在线人数',
  `4` int(10) NOT NULL DEFAULT '0' COMMENT '4时在线人数',
  `6` int(10) NOT NULL DEFAULT '0' COMMENT '6时在线人数',
  `8` int(10) NOT NULL DEFAULT '0' COMMENT '8时在线人数',
  `10` int(10) NOT NULL DEFAULT '0' COMMENT '10时在线人数',
  `12` int(10) NOT NULL DEFAULT '0' COMMENT '12时在线人数',
  `14` int(10) NOT NULL DEFAULT '0' COMMENT '14时在线人数',
  `16` int(10) NOT NULL DEFAULT '0' COMMENT '16时在线人数',
  `18` int(10) NOT NULL DEFAULT '0' COMMENT '18时在线人数',
  `20` int(10) NOT NULL DEFAULT '0' COMMENT '20时在线人数',
  `22` int(10) NOT NULL DEFAULT '0' COMMENT '22时在线人数',
  `24` int(10) NOT NULL DEFAULT '0' COMMENT '24时在线人数',
  PRIMARY KEY (`time`),
  KEY `time` (`time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of onlines_log
-- ----------------------------

-- ----------------------------
-- Table structure for `openid`
-- ----------------------------
DROP TABLE IF EXISTS `openid`;
CREATE TABLE `openid` (
  `id` bigint(20) NOT NULL DEFAULT '0',
  `login_appid` varchar(40) DEFAULT NULL COMMENT '登录APPID',
  `login_openid` varchar(40) DEFAULT NULL COMMENT '登录OPENID',
  `pay_appid` varchar(40) DEFAULT NULL COMMENT '支付APPID',
  `pay_openid` varchar(40) DEFAULT NULL COMMENT '支付OPENID',
  `red_appid` varchar(40) DEFAULT NULL COMMENT '企业付款APPID',
  `red_openid` varchar(40) DEFAULT NULL COMMENT '企业付款OPENID',
  `role_id` int(11) DEFAULT '0' COMMENT '玩家ID',
  `unionid` varchar(40) DEFAULT NULL COMMENT '微信开放平台UNIONID',
  `create_time` datetime DEFAULT NULL COMMENT '注册时间',
  PRIMARY KEY (`id`),
  KEY `index_name` (`red_openid`,`login_openid`,`pay_openid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of openid
-- ----------------------------

-- ----------------------------
-- Table structure for `phone_log`
-- ----------------------------
DROP TABLE IF EXISTS `phone_log`;
CREATE TABLE `phone_log` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `role_id` int(10) unsigned NOT NULL,
  `item_id` int(10) unsigned NOT NULL DEFAULT '0',
  `create_time` int(10) unsigned NOT NULL,
  `val` int(10) unsigned NOT NULL,
  `phone` varchar(40) NOT NULL,
  `time` int(10) unsigned NOT NULL DEFAULT '0',
  `order_id` text NOT NULL,
  `state` int(10) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of phone_log
-- ----------------------------

-- ----------------------------
-- Table structure for `phone_prize`
-- ----------------------------
DROP TABLE IF EXISTS `phone_prize`;
CREATE TABLE `phone_prize` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `goods_id` int(10) unsigned NOT NULL,
  `num` int(10) unsigned NOT NULL DEFAULT '5',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=18 DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of phone_prize
-- ----------------------------
INSERT INTO `phone_prize` VALUES ('9', '22', '4');
INSERT INTO `phone_prize` VALUES ('10', '48', '2');
INSERT INTO `phone_prize` VALUES ('11', '47', '0');
INSERT INTO `phone_prize` VALUES ('12', '45', '4');
INSERT INTO `phone_prize` VALUES ('13', '43', '0');
INSERT INTO `phone_prize` VALUES ('14', '44', '3');
INSERT INTO `phone_prize` VALUES ('15', '26', '4');
INSERT INTO `phone_prize` VALUES ('16', '23', '4');
INSERT INTO `phone_prize` VALUES ('17', '55', '5');

-- ----------------------------
-- Table structure for `pot`
-- ----------------------------
DROP TABLE IF EXISTS `pot`;
CREATE TABLE `pot` (
  `id` int(11) unsigned NOT NULL,
  `in` bigint(20) NOT NULL DEFAULT '0',
  `out` bigint(20) NOT NULL DEFAULT '0',
  `under` bigint(20) NOT NULL DEFAULT '0' COMMENT '垫分',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of pot
-- ----------------------------
INSERT INTO `pot` VALUES ('0', '0', '0', '0');
INSERT INTO `pot` VALUES ('1', '0', '0', '0');
INSERT INTO `pot` VALUES ('2', '0', '0', '0');
INSERT INTO `pot` VALUES ('3', '0', '0', '0');
INSERT INTO `pot` VALUES ('4', '0', '0', '0');
INSERT INTO `pot` VALUES ('5', '0', '0', '0');
INSERT INTO `pot` VALUES ('6', '0', '0', '0');

-- ----------------------------
-- Table structure for `prize_1`
-- ----------------------------
DROP TABLE IF EXISTS `prize_1`;
CREATE TABLE `prize_1` (
  `id` int(11) NOT NULL COMMENT '奖品ID',
  `all_1` int(11) NOT NULL DEFAULT '0' COMMENT '档位1夹的总次数',
  `hit_1` int(11) NOT NULL DEFAULT '0' COMMENT '档位1夹中次数',
  `all_2` int(11) NOT NULL DEFAULT '0',
  `hit_2` int(11) NOT NULL DEFAULT '0',
  `all_3` int(11) NOT NULL DEFAULT '0',
  `hit_3` int(11) NOT NULL DEFAULT '0',
  `all_4` int(11) NOT NULL DEFAULT '0',
  `hit_4` int(11) NOT NULL DEFAULT '0',
  `all_5` int(11) NOT NULL DEFAULT '0',
  `hit_5` int(11) NOT NULL DEFAULT '0',
  `all_6` int(11) NOT NULL DEFAULT '0',
  `hit_6` int(11) NOT NULL DEFAULT '0',
  `all_7` int(11) NOT NULL DEFAULT '0',
  `hit_7` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of prize_1
-- ----------------------------
INSERT INTO `prize_1` VALUES ('16', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('17', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('18', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('22', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('23', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('27', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('28', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('32', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('33', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('34', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('38', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('45', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('46', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('47', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('51', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('64', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('68', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('71', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('72', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('73', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('74', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('75', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('76', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('77', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('79', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('80', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('82', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('83', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('85', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('86', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('87', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('88', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('89', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('90', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('91', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('92', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('93', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('94', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('95', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('98', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('99', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('122', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('123', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('124', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('125', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('126', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('127', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('128', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('129', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('130', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('131', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('132', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('134', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('146', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('147', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('148', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('149', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('150', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('151', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('152', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('153', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('154', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('155', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('156', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('157', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('158', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('159', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');
INSERT INTO `prize_1` VALUES ('160', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0');

-- ----------------------------
-- Table structure for `profit_log`
-- ----------------------------
DROP TABLE IF EXISTS `profit_log`;
CREATE TABLE `profit_log` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `time` int(11) NOT NULL COMMENT '时间',
  `role_id` int(11) NOT NULL,
  `openid` text,
  `profit` bigint(20) NOT NULL DEFAULT '0' COMMENT '盈利值',
  `win` int(11) NOT NULL DEFAULT '0' COMMENT '奖励值',
  `type` int(11) NOT NULL COMMENT '奖励类型',
  `state` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '0->未发奖1->已发奖',
  PRIMARY KEY (`id`),
  KEY `role_id` (`role_id`),
  KEY `time` (`time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of profit_log
-- ----------------------------

-- ----------------------------
-- Table structure for `profit_setting`
-- ----------------------------
DROP TABLE IF EXISTS `profit_setting`;
CREATE TABLE `profit_setting` (
  `id` int(11) NOT NULL,
  `next_id` int(11) NOT NULL DEFAULT '0',
  `section` varchar(40) DEFAULT NULL,
  `val` int(11) NOT NULL DEFAULT '0' COMMENT '奖励金额',
  `type` int(11) NOT NULL DEFAULT '1' COMMENT '1->balance,2->rmb',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of profit_setting
-- ----------------------------
INSERT INTO `profit_setting` VALUES ('1', '2', '[5000,10000]', '1000', '1');
INSERT INTO `profit_setting` VALUES ('2', '3', '[10000,30000]', '2000', '1');
INSERT INTO `profit_setting` VALUES ('3', '4', '[30000,50000]', '3000', '1');
INSERT INTO `profit_setting` VALUES ('4', '5', '[50000,100000]', '5000', '1');
INSERT INTO `profit_setting` VALUES ('5', '6', '[100000,300000]', '8000', '1');
INSERT INTO `profit_setting` VALUES ('6', '7', '[300000,500000]', '16000', '1');
INSERT INTO `profit_setting` VALUES ('7', '8', '[500000,1000000]', '18000', '2');
INSERT INTO `profit_setting` VALUES ('8', '9', '[1000000,3000000]', '48000', '2');
INSERT INTO `profit_setting` VALUES ('9', '10', '[3000000,5000000]', '128000', '2');
INSERT INTO `profit_setting` VALUES ('10', '11', '[5000000,10000000]', '188000', '2');
INSERT INTO `profit_setting` VALUES ('11', '12', '[10000000,15000000]', '288000', '2');
INSERT INTO `profit_setting` VALUES ('12', '0', '15000000', '488000', '2');

-- ----------------------------
-- Table structure for `rank`
-- ----------------------------
DROP TABLE IF EXISTS `rank`;
CREATE TABLE `rank` (
  `id` int(10) unsigned NOT NULL,
  `profit_rank` mediumtext NOT NULL,
  `rich_rank` longtext NOT NULL,
  `time` int(10) unsigned NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of rank
-- ----------------------------
INSERT INTO `rank` VALUES ('1', '[]', '[]', '0');

-- ----------------------------
-- Table structure for `recharge_log`
-- ----------------------------
DROP TABLE IF EXISTS `recharge_log`;
CREATE TABLE `recharge_log` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `order_id` varchar(40) NOT NULL DEFAULT '0' COMMENT '商户订单号',
  `role_id` int(10) unsigned NOT NULL,
  `name` varchar(40) NOT NULL,
  `val` int(10) unsigned NOT NULL COMMENT '充值金额',
  `time` int(10) unsigned DEFAULT NULL COMMENT '充值时间',
  `charm` int(11) NOT NULL DEFAULT '0' COMMENT '充值魅力值',
  `gold` int(10) NOT NULL COMMENT '充值金豆',
  `charge_id` text NOT NULL,
  `state` tinyint(2) NOT NULL DEFAULT '0' COMMENT '0=申请支付  1=支付完成',
  `type` int(11) NOT NULL,
  `gift` int(10) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `id` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of recharge_log
-- ----------------------------

-- ----------------------------
-- Table structure for `red_envelope`
-- ----------------------------
DROP TABLE IF EXISTS `red_envelope`;
CREATE TABLE `red_envelope` (
  `id` int(10) NOT NULL AUTO_INCREMENT,
  `role_id` int(11) NOT NULL DEFAULT '0',
  `reward_id` int(10) NOT NULL COMMENT '对应的奖品ID',
  `name` text NOT NULL,
  `red_id` text NOT NULL COMMENT '红包ID',
  `create_time` int(11) NOT NULL DEFAULT '0' COMMENT '创建时间',
  `order_id` varchar(40) NOT NULL DEFAULT '0' COMMENT '订单商户号',
  `amount` int(11) NOT NULL DEFAULT '0' COMMENT '金额（单位：分）',
  `state` int(4) NOT NULL DEFAULT '0' COMMENT '0=创建，1=发送，2=领取',
  PRIMARY KEY (`id`),
  KEY `reward_id` (`reward_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of red_envelope
-- ----------------------------

-- ----------------------------
-- Table structure for `report`
-- ----------------------------
DROP TABLE IF EXISTS `report`;
CREATE TABLE `report` (
  `id` int(10) unsigned NOT NULL,
  `1` int(10) unsigned NOT NULL DEFAULT '0',
  `2` int(10) unsigned NOT NULL DEFAULT '0',
  `3` int(10) unsigned NOT NULL DEFAULT '0',
  `4` int(10) unsigned NOT NULL DEFAULT '0',
  `5` int(10) unsigned NOT NULL DEFAULT '0',
  `6` int(10) unsigned NOT NULL DEFAULT '0',
  `7` int(10) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of report
-- ----------------------------

-- ----------------------------
-- Table structure for `reward`
-- ----------------------------
DROP TABLE IF EXISTS `reward`;
CREATE TABLE `reward` (
  `id` int(10) NOT NULL,
  `source` int(11) NOT NULL DEFAULT '1' COMMENT '1->夹娃娃，2->商城',
  `order_id` varchar(40) NOT NULL COMMENT '订单ID，全服唯一',
  `role_id` int(11) NOT NULL,
  `room_id` int(4) NOT NULL DEFAULT '0',
  `item_id` int(6) NOT NULL,
  `item_name` varchar(40) NOT NULL,
  `picture` varchar(999) DEFAULT NULL,
  `item_type` int(6) NOT NULL COMMENT '1=金币奖励；2=实物奖励',
  `time` int(11) NOT NULL DEFAULT '0',
  `receive_time` int(11) NOT NULL DEFAULT '0' COMMENT '领取时间',
  `status` int(11) NOT NULL COMMENT '0=未领取；1=已领取',
  `name` varchar(40) DEFAULT NULL,
  `phone` varchar(20) DEFAULT NULL,
  `addr` varchar(60) DEFAULT NULL,
  `hit_val` int(11) NOT NULL DEFAULT '0' COMMENT '投入金额',
  `val` bigint(20) NOT NULL DEFAULT '0' COMMENT '价值（对应prize的val）',
  `rmb` int(10) unsigned DEFAULT '0',
  `nickname` varchar(50) DEFAULT NULL,
  `icon` varchar(255) DEFAULT NULL,
  `state` int(10) unsigned NOT NULL DEFAULT '0',
  `agent_id` int(10) unsigned NOT NULL DEFAULT '0',
  `words` text COMMENT '获奖感言',
  `share_val` int(11) NOT NULL DEFAULT '0' COMMENT '晒单奖励',
  `vip` int(11) NOT NULL DEFAULT '0',
  `prize_id` int(11) NOT NULL DEFAULT '0',
  `robot` int(10) unsigned NOT NULL DEFAULT '0',
  `red_id` varchar(40) DEFAULT NULL,
  `red_order_id` varchar(40) DEFAULT NULL,
  `exchange_type` int(10) unsigned NOT NULL DEFAULT '1'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of reward
-- ----------------------------
INSERT INTO `reward` VALUES ('1', '1', 'A1523515477200031301', '20003', '2', '130', '3元红包', null, '4', '1523515477', '1523515477', '1', null, null, null, '500', '3000', '3', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '6', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('2', '2', 'A152351856954398620003', '20003', '0', '9', '惠普cp1200打印机', '/company_api/public/goods/prize_printer.png', '5', '1523518569', '1523518569', '1', '111', '11111111111', '111', '1200000', '1200000', '1000', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '0', '0', null, '0', '0', '5', '0', null, null, '1');
INSERT INTO `reward` VALUES ('3', '2', 'ZC20003152351935821598611', '20003', '0', '47', '20元话费', '/company_api/public/goods/prize_phone_card.png', '2', '1523519358', '1523519358', '1', '', '11111111111', '', '20000', '20000', '20', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '2', '0', null, '0', '0', '3', '0', null, null, '2');
INSERT INTO `reward` VALUES ('4', '2', 'A152351982615298620003', '20003', '0', '9', '惠普cp1200打印机', '/company_api/public/goods/prize_printer.png', '5', '1523519826', '1523519826', '1', '111', '11111111111', '111', '1200000', '1200000', '1000', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '0', '0', null, '0', '0', '5', '0', null, null, '1');
INSERT INTO `reward` VALUES ('5', '2', 'A152352206729398620003', '20003', '0', '9', '惠普cp1200打印机', '/company_api/public/goods/prize_printer.png', '5', '1523522067', '1523522067', '1', '111', '11111111111', '111', '1200000', '1200000', '1000', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '0', '0', null, '0', '0', '5', '0', null, null, '1');
INSERT INTO `reward` VALUES ('6', '2', 'A152352207737198620003', '20003', '0', '9', '惠普cp1200打印机', '/company_api/public/goods/prize_printer.png', '5', '1523522077', '1523522077', '1', '111', '11111111111', '111', '1200000', '1200000', '1000', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '0', '0', null, '0', '0', '5', '0', null, null, '1');
INSERT INTO `reward` VALUES ('7', '2', 'ZC20003152352209749698611', '20003', '0', '47', '20元话费', '/company_api/public/goods/prize_phone_card.png', '2', '1523522097', '1523522097', '1', '', '11111111111', '', '20000', '20000', '20', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '2', '0', null, '0', '0', '3', '0', null, null, '2');
INSERT INTO `reward` VALUES ('8', '2', 'ZC20003152352221230998611', '20003', '0', '47', '20元话费', '/company_api/public/goods/prize_phone_card.png', '2', '1523522212', '1523522212', '1', '', '11111111111', '', '24000', '24000', '20', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '2', '0', null, '0', '0', '3', '0', null, null, '1');
INSERT INTO `reward` VALUES ('9', '2', 'ZC20003152352245607498613', '20003', '0', '43', '50元京东卡', '/company_api/public/goods/prize_jd_card.png', '3', '1523522456', '1523522456', '1', '', '', '', '50000', '50000', '50', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '0', null, '0', '0', '2', '0', null, null, '2');
INSERT INTO `reward` VALUES ('10', '2', 'ZC20003152352248180998613', '20003', '0', '43', '50元京东卡', '/company_api/public/goods/prize_jd_card.png', '3', '1523522481', '1523522481', '1', '', '', '', '60000', '60000', '50', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '0', null, '0', '0', '2', '0', null, null, '1');
INSERT INTO `reward` VALUES ('11', '2', 'ZC20003152352250638798613', '20003', '0', '43', '50元京东卡', '/company_api/public/goods/prize_jd_card.png', '3', '1523522506', '1523522506', '1', '', '', '', '50000', '50000', '50', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '0', null, '0', '0', '2', '0', null, null, '2');
INSERT INTO `reward` VALUES ('12', '2', 'ZC20003152352252257498610', '20003', '0', '48', '30元话费', '/company_api/public/goods/prize_phone_card.png', '2', '1523522522', '1523522522', '1', '', '11111111111', '', '30000', '30000', '30', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '2', '0', null, '0', '0', '3', '0', null, null, '2');
INSERT INTO `reward` VALUES ('13', '2', 'A152352254201298620003', '20003', '0', '14', '中国黄金5g', '/company_api/public/goods/prize_china_gold.png', '5', '1523522542', '1523522542', '1', '111', '11111111111', '111', '1800000', '1800000', '1500', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '0', '0', null, '0', '0', '10', '0', null, null, '1');
INSERT INTO `reward` VALUES ('14', '3', 'A152352389220008014', '20008', '0', '0', '50元福利红包', null, '4', '1523523892', '1523523892', '1', '', '', '', '0', '50000', '50', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '4', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('15', '3', 'A152352390920008015', '20008', '0', '0', '49元福利红包', null, '4', '1523523909', '1523523909', '1', '', '', '', '0', '49000', '49', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '4', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('16', '3', 'A152352391120008016', '20008', '0', '0', '49元福利红包', null, '4', '1523523911', '1523523911', '1', '', '', '', '0', '49000', '49', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '4', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('17', '3', 'A152352392920008017', '20008', '0', '0', '48元福利红包', null, '4', '1523523929', '1523523929', '1', '', '', '', '0', '48000', '48', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '4', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('18', '4', 'A152352551620003018', '20003', '0', '0', '488盈利榜红包', null, '4', '1523525516', '1523525516', '1', '', '11111111111', '', '0', '488000', '488', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '4', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('19', '1', 'A15235270262000313119', '20003', '2', '131', '5元红包', null, '4', '1523527026', '1523527026', '1', null, null, null, '500', '5000', '5', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '6', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('20', '1', 'A15235286852000312620', '20003', '1', '126', '1元红包', null, '4', '1523528685', '1523528685', '1', null, null, null, '100', '1000', '1', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '6', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('21', '1', 'A15235318052000812621', '20008', '1', '126', '1元红包', null, '4', '1523531805', '1523531805', '1', null, null, null, '100', '1000', '1', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('22', '1', 'A15235320122000312822', '20003', '1', '128', '288金豆', null, '1', '1523532012', '1523532012', '1', null, null, null, '100', '288', '0', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '1', '0', null, null, '1');
INSERT INTO `reward` VALUES ('23', '1', 'A15235320192000815923', '20008', '1', '159', '10奖券', null, '8', '1523532019', '1523532019', '1', null, null, null, '100', '1200', '1', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '14', '0', null, null, '1');
INSERT INTO `reward` VALUES ('24', '1', 'A15235320932000312624', '20003', '1', '126', '1元红包', null, '4', '1523532093', '1523532093', '1', null, null, null, '100', '1000', '1', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('25', '1', 'A15235322352000812625', '20008', '1', '126', '1元红包', null, '4', '1523532235', '1523532235', '1', null, null, null, '100', '1000', '1', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('26', '1', 'A15235323102000812326', '20008', '1', '123', '3元红包', null, '4', '1523532310', '1523532310', '1', null, null, null, '100', '3000', '3', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('27', '1', 'A15235349522000312627', '20003', '1', '126', '1元红包', null, '4', '1523534952', '1523534952', '1', null, null, null, '100', '1000', '1', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('28', '1', 'A15235357292000812628', '20008', '1', '126', '1元红包', null, '4', '1523535729', '1523535729', '1', null, null, null, '100', '1000', '1', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('29', '1', 'A1523930422200039129', '20003', '6', '91', '23888金豆', null, '1', '1523930422', '1523930422', '1', null, null, null, '10000', '23888', '0', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '1', '0', null, null, '1');
INSERT INTO `reward` VALUES ('30', '1', 'A15239304712000314730', '20003', '6', '147', '15元红包', null, '4', '1523930471', '1523930471', '1', null, null, null, '10000', '15000', '15', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('31', '1', 'A15239305592000314731', '20003', '6', '147', '15元红包', null, '4', '1523930559', '1523930559', '1', null, null, null, '10000', '15000', '15', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('32', '1', 'A15239305692000314632', '20003', '6', '146', '25元红包', null, '4', '1523930569', '1523930569', '1', null, null, null, '10000', '25000', '25', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('33', '1', 'A15239306392000314733', '20003', '6', '147', '15元红包', null, '4', '1523930639', '1523930639', '1', null, null, null, '10000', '15000', '15', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('34', '1', 'A15239311432000314634', '20003', '6', '146', '25元红包', null, '4', '1523931143', '1523931143', '1', null, null, null, '10000', '25000', '25', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('35', '1', 'A15239312222000315035', '20003', '3', '150', '5元红包', null, '4', '1523931222', '1523931222', '1', null, null, null, '1000', '5000', '5', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('36', '1', 'A1523931311200031936', '20003', '0', '19', '9元红包', null, '4', '1523931311', '1523931311', '1', '', '', '', '0', '9000', '9', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '4', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('37', '1', 'A15239315392000315537', '20003', '5', '155', '8元红包', null, '4', '1523931539', '1523931539', '1', null, null, null, '5000', '8000', '8', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('38', '1', 'A15239315602000315538', '20003', '5', '155', '8元红包', null, '4', '1523931560', '1523931560', '1', null, null, null, '5000', '8000', '8', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('39', '1', 'A15239315802000315539', '20003', '5', '155', '8元红包', null, '4', '1523931580', '1523931580', '1', null, null, null, '5000', '8000', '8', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('40', '1', 'A15239345642000314740', '20003', '6', '147', '15元红包', null, '4', '1523934564', '1523934564', '1', null, null, null, '10000', '15000', '15', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('41', '1', 'A15239345852000314741', '20003', '6', '147', '15元红包', null, '4', '1523934585', '1523934585', '1', null, null, null, '10000', '15000', '15', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('42', '1', 'A15239594622000314642', '20003', '6', '146', '25元红包', null, '4', '1523959462', '1523959462', '1', null, null, null, '10000', '25000', '25', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '4', '0', null, null, '1');
INSERT INTO `reward` VALUES ('43', '1', 'A15239647552000314743', '20003', '6', '147', '15元红包', null, '4', '1523964755', '1523964755', '1', null, null, null, '10000', '15000', '15', '?????จุ๊บ', 'http://wx.qlogo.cn/mmopen/Xyuqkk3foUfL0ibicgTiaKDOUgpiaTEppibhWibc38jgcmmGxPhZeroeWqGwNrbypP9ovnJFIcq5EicMI2zdW4a5nlMYIg2VWWyHzom/0', '1', '1', null, '0', '0', '4', '0', null, null, '1');

-- ----------------------------
-- Table structure for `reward_log`
-- ----------------------------
DROP TABLE IF EXISTS `reward_log`;
CREATE TABLE `reward_log` (
  `id` int(10) NOT NULL,
  `order_id` varchar(40) NOT NULL COMMENT '订单ID，全服唯一',
  `role_id` int(11) NOT NULL,
  `room_id` int(4) NOT NULL,
  `item_id` int(6) NOT NULL,
  `item_name` varchar(40) NOT NULL,
  `item_type` int(6) NOT NULL COMMENT '1=金币奖励；2=实物奖励',
  `time` int(11) NOT NULL DEFAULT '0',
  `status` int(11) NOT NULL COMMENT '0=未领取；1=已领取',
  `vip` int(11) NOT NULL DEFAULT '0',
  `hit_val` int(10) unsigned NOT NULL DEFAULT '0',
  `prize_id` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of reward_log
-- ----------------------------

-- ----------------------------
-- Table structure for `robot`
-- ----------------------------
DROP TABLE IF EXISTS `robot`;
CREATE TABLE `robot` (
  `role_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `nickname` text NOT NULL,
  `icon` text NOT NULL,
  `balance` bigint(20) unsigned NOT NULL DEFAULT '0',
  `deposit` bigint(20) unsigned NOT NULL DEFAULT '0',
  `withdraw` bigint(20) unsigned NOT NULL DEFAULT '0',
  `profit` bigint(20) NOT NULL DEFAULT '0',
  `vip` int(10) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`role_id`)
) ENGINE=InnoDB AUTO_INCREMENT=301 DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of robot
-- ----------------------------
INSERT INTO `robot` VALUES ('1', '时间煮雨', 'http://www.zhongchuanxinxi.com/dicehead/1.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('2', '映蓉', 'http://www.zhongchuanxinxi.com/dicehead/2.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('3', '丽琴', 'http://www.zhongchuanxinxi.com/dicehead/3.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('4', '微微', 'http://www.zhongchuanxinxi.com/dicehead/4.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('5', 'Emma', 'http://www.zhongchuanxinxi.com/dicehead/5.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('6', 'LL', 'http://www.zhongchuanxinxi.com/dicehead/6.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('7', '琳', 'http://www.zhongchuanxinxi.com/dicehead/7.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('8', '棋', 'http://www.zhongchuanxinxi.com/dicehead/8.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('9', 'ameko', 'http://www.zhongchuanxinxi.com/dicehead/9.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('10', '小洁', 'http://www.zhongchuanxinxi.com/dicehead/10.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('11', '天涯小锅女', 'http://www.zhongchuanxinxi.com/dicehead/11.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('12', '莫思思', 'http://www.zhongchuanxinxi.com/dicehead/12.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('13', 'Becky', 'http://www.zhongchuanxinxi.com/dicehead/13.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('14', '王盈', 'http://www.zhongchuanxinxi.com/dicehead/14.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('15', 'may may酱', 'http://www.zhongchuanxinxi.com/dicehead/15.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('16', '小娅', 'http://www.zhongchuanxinxi.com/dicehead/16.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('17', '徐晓', 'http://www.zhongchuanxinxi.com/dicehead/17.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('18', 'ZCF', 'http://www.zhongchuanxinxi.com/dicehead/18.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('19', '馨儿', 'http://www.zhongchuanxinxi.com/dicehead/19.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('20', '田小鹿', 'http://www.zhongchuanxinxi.com/dicehead/20.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('21', '琉璃苣', 'http://www.zhongchuanxinxi.com/dicehead/21.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('22', '七格格', 'http://www.zhongchuanxinxi.com/dicehead/22.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('23', 'Icy', 'http://www.zhongchuanxinxi.com/dicehead/23.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('24', '杨大仙', 'http://www.zhongchuanxinxi.com/dicehead/24.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('25', '瑛子', 'http://www.zhongchuanxinxi.com/dicehead/25.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('26', '文菲', 'http://www.zhongchuanxinxi.com/dicehead/26.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('27', '姬小晨', 'http://www.zhongchuanxinxi.com/dicehead/27.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('28', 'CherryC', 'http://www.zhongchuanxinxi.com/dicehead/28.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('29', '钢牙妹', 'http://www.zhongchuanxinxi.com/dicehead/29.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('30', 'JAMIE ', 'http://www.zhongchuanxinxi.com/dicehead/30.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('31', 'Daisy', 'http://www.zhongchuanxinxi.com/dicehead/31.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('32', '欣瑶', 'http://www.zhongchuanxinxi.com/dicehead/32.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('33', '七七小巫婆', 'http://www.zhongchuanxinxi.com/dicehead/33.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('34', '菲', 'http://www.zhongchuanxinxi.com/dicehead/34.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('35', '瞌睡大懒虫', 'http://www.zhongchuanxinxi.com/dicehead/35.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('36', '走猫步的猪', 'http://www.zhongchuanxinxi.com/dicehead/36.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('37', 'Julie', 'http://www.zhongchuanxinxi.com/dicehead/37.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('38', '潘潘', 'http://www.zhongchuanxinxi.com/dicehead/38.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('39', '桃子小姐', 'http://www.zhongchuanxinxi.com/dicehead/39.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('40', '竹云', 'http://www.zhongchuanxinxi.com/dicehead/40.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('41', 'Celia', 'http://www.zhongchuanxinxi.com/dicehead/41.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('42', 'Yan', 'http://www.zhongchuanxinxi.com/dicehead/42.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('43', '余余', 'http://www.zhongchuanxinxi.com/dicehead/43.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('44', '李大小姐', 'http://www.zhongchuanxinxi.com/dicehead/44.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('45', '米儿', 'http://www.zhongchuanxinxi.com/dicehead/45.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('46', '杨霞', 'http://www.zhongchuanxinxi.com/dicehead/46.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('47', '梦梦', 'http://www.zhongchuanxinxi.com/dicehead/47.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('48', '西西小可', 'http://www.zhongchuanxinxi.com/dicehead/48.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('49', '潘潘', 'http://www.zhongchuanxinxi.com/dicehead/49.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('50', '张小雅', 'http://www.zhongchuanxinxi.com/dicehead/50.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('51', 'Lily', 'http://www.zhongchuanxinxi.com/dicehead/51.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('52', '小倩', 'http://www.zhongchuanxinxi.com/dicehead/52.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('53', '小娅', 'http://www.zhongchuanxinxi.com/dicehead/53.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('54', '陶小妞', 'http://www.zhongchuanxinxi.com/dicehead/54.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('55', '吃小梨', 'http://www.zhongchuanxinxi.com/dicehead/55.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('56', 'Ruby', 'http://www.zhongchuanxinxi.com/dicehead/56.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('57', 'LOVEの大脸猫', 'http://www.zhongchuanxinxi.com/dicehead/57.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('58', '懒猪', 'http://www.zhongchuanxinxi.com/dicehead/58.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('59', 'Silvia', 'http://www.zhongchuanxinxi.com/dicehead/59.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('60', '詹氏帮掌门', 'http://www.zhongchuanxinxi.com/dicehead/60.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('61', 'bie bie熊', 'http://www.zhongchuanxinxi.com/dicehead/61.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('62', 'Candy', 'http://www.zhongchuanxinxi.com/dicehead/62.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('63', 'Rain虹', 'http://www.zhongchuanxinxi.com/dicehead/63.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('64', '凉生', 'http://www.zhongchuanxinxi.com/dicehead/64.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('65', '縌埘针', 'http://www.zhongchuanxinxi.com/dicehead/65.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('66', '张小希', 'http://www.zhongchuanxinxi.com/dicehead/66.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('67', 'Zoey Zhou', 'http://www.zhongchuanxinxi.com/dicehead/67.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('68', '唐婉', 'http://www.zhongchuanxinxi.com/dicehead/68.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('69', '翾子花生', 'http://www.zhongchuanxinxi.com/dicehead/69.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('70', '娜娜', 'http://www.zhongchuanxinxi.com/dicehead/70.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('71', '黄燕', 'http://www.zhongchuanxinxi.com/dicehead/71.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('72', '何婷', 'http://www.zhongchuanxinxi.com/dicehead/72.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('73', '莹莹', 'http://www.zhongchuanxinxi.com/dicehead/73.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('74', '晓菊', 'http://www.zhongchuanxinxi.com/dicehead/74.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('75', '君君', 'http://www.zhongchuanxinxi.com/dicehead/75.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('76', '张苦橙', 'http://www.zhongchuanxinxi.com/dicehead/76.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('77', '彭娟', 'http://www.zhongchuanxinxi.com/dicehead/77.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('78', '斯斯', 'http://www.zhongchuanxinxi.com/dicehead/78.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('79', 'Alisa小霞', 'http://www.zhongchuanxinxi.com/dicehead/79.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('80', '月佥', 'http://www.zhongchuanxinxi.com/dicehead/80.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('81', '夏芳', 'http://www.zhongchuanxinxi.com/dicehead/81.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('82', 'FernH', 'http://www.zhongchuanxinxi.com/dicehead/82.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('83', 'lucky', 'http://www.zhongchuanxinxi.com/dicehead/83.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('84', 'ssy', 'http://www.zhongchuanxinxi.com/dicehead/84.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('85', '林小玲', 'http://www.zhongchuanxinxi.com/dicehead/85.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('86', '刘晓倩', 'http://www.zhongchuanxinxi.com/dicehead/86.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('87', '音', 'http://www.zhongchuanxinxi.com/dicehead/87.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('88', '希希', 'http://www.zhongchuanxinxi.com/dicehead/88.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('89', '王露', 'http://www.zhongchuanxinxi.com/dicehead/89.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('90', '潇潇', 'http://www.zhongchuanxinxi.com/dicehead/90.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('91', 'xuandu', 'http://www.zhongchuanxinxi.com/dicehead/91.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('92', '小唯', 'http://www.zhongchuanxinxi.com/dicehead/92.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('93', '桃子', 'http://www.zhongchuanxinxi.com/dicehead/93.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('94', '云想', 'http://www.zhongchuanxinxi.com/dicehead/94.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('95', '廖云秋', 'http://www.zhongchuanxinxi.com/dicehead/95.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('96', 'bobo', 'http://www.zhongchuanxinxi.com/dicehead/96.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('97', 'Fenny', 'http://www.zhongchuanxinxi.com/dicehead/97.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('98', '朵儿', 'http://www.zhongchuanxinxi.com/dicehead/98.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('99', 'Aviva', 'http://www.zhongchuanxinxi.com/dicehead/99.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('100', 'momo毛毛', 'http://www.zhongchuanxinxi.com/dicehead/100.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('101', '晴', 'http://www.zhongchuanxinxi.com/dicehead/101.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('102', '可苗', 'http://www.zhongchuanxinxi.com/dicehead/102.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('103', 'YOYO', 'http://www.zhongchuanxinxi.com/dicehead/103.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('104', '时光', 'http://www.zhongchuanxinxi.com/dicehead/104.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('105', '娜娜', 'http://www.zhongchuanxinxi.com/dicehead/105.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('106', '小雪', 'http://www.zhongchuanxinxi.com/dicehead/106.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('107', 'JeaNy', 'http://www.zhongchuanxinxi.com/dicehead/107.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('108', '杨霞', 'http://www.zhongchuanxinxi.com/dicehead/108.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('109', 'aki', 'http://www.zhongchuanxinxi.com/dicehead/109.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('110', '宝熊公主', 'http://www.zhongchuanxinxi.com/dicehead/110.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('111', '陆小米', 'http://www.zhongchuanxinxi.com/dicehead/111.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('112', '流浪兔', 'http://www.zhongchuanxinxi.com/dicehead/112.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('113', '马熙儿', 'http://www.zhongchuanxinxi.com/dicehead/113.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('114', 'Wendy', 'http://www.zhongchuanxinxi.com/dicehead/114.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('115', 'Ce|esтe', 'http://www.zhongchuanxinxi.com/dicehead/115.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('116', '樂猫', 'http://www.zhongchuanxinxi.com/dicehead/116.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('117', 'todocolor', 'http://www.zhongchuanxinxi.com/dicehead/117.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('118', '徐小夏', 'http://www.zhongchuanxinxi.com/dicehead/118.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('119', 'Tracy', 'http://www.zhongchuanxinxi.com/dicehead/119.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('120', '钟颖儿', 'http://www.zhongchuanxinxi.com/dicehead/120.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('121', 'Shado', 'http://www.zhongchuanxinxi.com/dicehead/121.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('122', '木子暁筑', 'http://www.zhongchuanxinxi.com/dicehead/122.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('123', '空白格', 'http://www.zhongchuanxinxi.com/dicehead/123.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('124', '小美', 'http://www.zhongchuanxinxi.com/dicehead/124.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('125', 'Barbara', 'http://www.zhongchuanxinxi.com/dicehead/125.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('126', 'baby cat', 'http://www.zhongchuanxinxi.com/dicehead/126.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('127', '林淇', 'http://www.zhongchuanxinxi.com/dicehead/127.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('128', '熙雅', 'http://www.zhongchuanxinxi.com/dicehead/128.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('129', 'Xenia', 'http://www.zhongchuanxinxi.com/dicehead/129.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('130', '月慧', 'http://www.zhongchuanxinxi.com/dicehead/130.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('131', '怡', 'http://www.zhongchuanxinxi.com/dicehead/131.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('132', 'Estelle', 'http://www.zhongchuanxinxi.com/dicehead/132.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('133', '李萌萌', 'http://www.zhongchuanxinxi.com/dicehead/133.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('134', '月野兔', 'http://www.zhongchuanxinxi.com/dicehead/134.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('135', '木子琳', 'http://www.zhongchuanxinxi.com/dicehead/135.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('136', '花花酱', 'http://www.zhongchuanxinxi.com/dicehead/136.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('137', '孙女侠', 'http://www.zhongchuanxinxi.com/dicehead/137.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('138', '白茶', 'http://www.zhongchuanxinxi.com/dicehead/138.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('139', '小琳萌', 'http://www.zhongchuanxinxi.com/dicehead/139.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('140', 'LYL', 'http://www.zhongchuanxinxi.com/dicehead/140.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('141', '不二琪琪', 'http://www.zhongchuanxinxi.com/dicehead/141.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('142', '姗姗', 'http://www.zhongchuanxinxi.com/dicehead/142.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('143', '梁子', 'http://www.zhongchuanxinxi.com/dicehead/143.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('144', 'Theia', 'http://www.zhongchuanxinxi.com/dicehead/144.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('145', '童童', 'http://www.zhongchuanxinxi.com/dicehead/145.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('146', 'lamvivi', 'http://www.zhongchuanxinxi.com/dicehead/146.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('147', 'Rita', 'http://www.zhongchuanxinxi.com/dicehead/147.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('148', 'Q晴', 'http://www.zhongchuanxinxi.com/dicehead/148.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('149', '琴子', 'http://www.zhongchuanxinxi.com/dicehead/149.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('150', '吴静', 'http://www.zhongchuanxinxi.com/dicehead/150.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('151', '婧', 'http://www.zhongchuanxinxi.com/dicehead/151.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('152', 'Abby', 'http://www.zhongchuanxinxi.com/dicehead/152.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('153', '莎莎', 'http://www.zhongchuanxinxi.com/dicehead/153.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('154', '婉玲', 'http://www.zhongchuanxinxi.com/dicehead/154.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('155', '杨悦诗', 'http://www.zhongchuanxinxi.com/dicehead/155.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('156', 'Mandy', 'http://www.zhongchuanxinxi.com/dicehead/156.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('157', '囡囡', 'http://www.zhongchuanxinxi.com/dicehead/157.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('158', '小青椒', 'http://www.zhongchuanxinxi.com/dicehead/158.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('159', '文菲', 'http://www.zhongchuanxinxi.com/dicehead/159.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('160', '王越', 'http://www.zhongchuanxinxi.com/dicehead/160.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('161', '浅蓝', 'http://www.zhongchuanxinxi.com/dicehead/161.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('162', 'Liya', 'http://www.zhongchuanxinxi.com/dicehead/162.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('163', '圆圆觉主', 'http://www.zhongchuanxinxi.com/dicehead/163.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('164', 'Bella', 'http://www.zhongchuanxinxi.com/dicehead/164.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('165', '黎惠', 'http://www.zhongchuanxinxi.com/dicehead/165.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('166', '媛公子', 'http://www.zhongchuanxinxi.com/dicehead/166.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('167', '小猪呼噜', 'http://www.zhongchuanxinxi.com/dicehead/167.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('168', '梦雪', 'http://www.zhongchuanxinxi.com/dicehead/168.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('169', 'Elsa', 'http://www.zhongchuanxinxi.com/dicehead/169.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('170', '继小爱', 'http://www.zhongchuanxinxi.com/dicehead/170.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('171', '菁菁', 'http://www.zhongchuanxinxi.com/dicehead/171.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('172', '孟小英', 'http://www.zhongchuanxinxi.com/dicehead/172.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('173', 'yiikii', 'http://www.zhongchuanxinxi.com/dicehead/173.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('174', 'Christal ', 'http://www.zhongchuanxinxi.com/dicehead/174.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('175', '陌雨', 'http://www.zhongchuanxinxi.com/dicehead/175.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('176', '宝怡', 'http://www.zhongchuanxinxi.com/dicehead/176.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('177', '七格格', 'http://www.zhongchuanxinxi.com/dicehead/177.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('178', 'Mlsu素', 'http://www.zhongchuanxinxi.com/dicehead/178.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('179', 'cynthia', 'http://www.zhongchuanxinxi.com/dicehead/179.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('180', 'yaffa', 'http://www.zhongchuanxinxi.com/dicehead/180.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('181', '萌小希', 'http://www.zhongchuanxinxi.com/dicehead/181.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('182', '董小姐', 'http://www.zhongchuanxinxi.com/dicehead/182.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('183', '槿年', 'http://www.zhongchuanxinxi.com/dicehead/183.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('184', 'Nina', 'http://www.zhongchuanxinxi.com/dicehead/184.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('185', '苏可可', 'http://www.zhongchuanxinxi.com/dicehead/185.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('186', '米苏', 'http://www.zhongchuanxinxi.com/dicehead/186.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('187', 'Fiona', 'http://www.zhongchuanxinxi.com/dicehead/187.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('188', '可唯', 'http://www.zhongchuanxinxi.com/dicehead/188.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('189', 'Ashley', 'http://www.zhongchuanxinxi.com/dicehead/189.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('190', '捏捏', 'http://www.zhongchuanxinxi.com/dicehead/190.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('191', '馋猫', 'http://www.zhongchuanxinxi.com/dicehead/191.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('192', '冷小诺', 'http://www.zhongchuanxinxi.com/dicehead/192.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('193', '敏丹', 'http://www.zhongchuanxinxi.com/dicehead/193.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('194', '螢焱蟲', 'http://www.zhongchuanxinxi.com/dicehead/194.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('195', '妍', 'http://www.zhongchuanxinxi.com/dicehead/195.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('196', '猜不透', 'http://www.zhongchuanxinxi.com/dicehead/196.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('197', '单细胞生物', 'http://www.zhongchuanxinxi.com/dicehead/197.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('198', '小玉', 'http://www.zhongchuanxinxi.com/dicehead/198.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('199', 'Eve', 'http://www.zhongchuanxinxi.com/dicehead/199.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('200', '丫丫', 'http://www.zhongchuanxinxi.com/dicehead/200.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('201', '罗琳', 'http://www.zhongchuanxinxi.com/dicehead/201.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('202', '刘诗遥', 'http://www.zhongchuanxinxi.com/dicehead/202.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('203', '安妮.Q', 'http://www.zhongchuanxinxi.com/dicehead/203.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('204', '蓓蓓', 'http://www.zhongchuanxinxi.com/dicehead/204.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('205', '依依', 'http://www.zhongchuanxinxi.com/dicehead/205.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('206', '若琳', 'http://www.zhongchuanxinxi.com/dicehead/206.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('207', '梦醒了', 'http://www.zhongchuanxinxi.com/dicehead/207.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('208', '赵萌', 'http://www.zhongchuanxinxi.com/dicehead/208.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('209', 'Sally', 'http://www.zhongchuanxinxi.com/dicehead/209.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('210', '欧茜', 'http://www.zhongchuanxinxi.com/dicehead/210.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('211', 'OJ-罗', 'http://www.zhongchuanxinxi.com/dicehead/211.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('212', 'Luminous', 'http://www.zhongchuanxinxi.com/dicehead/212.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('213', 'RAGREA', 'http://www.zhongchuanxinxi.com/dicehead/213.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('214', 'Tang三藏', 'http://www.zhongchuanxinxi.com/dicehead/214.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('215', '半世流离', 'http://www.zhongchuanxinxi.com/dicehead/215.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('216', '江源', 'http://www.zhongchuanxinxi.com/dicehead/216.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('217', '品爷', 'http://www.zhongchuanxinxi.com/dicehead/217.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('218', 'Nairo', 'http://www.zhongchuanxinxi.com/dicehead/218.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('219', '然', 'http://www.zhongchuanxinxi.com/dicehead/219.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('220', 'WZD', 'http://www.zhongchuanxinxi.com/dicehead/220.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('221', '超', 'http://www.zhongchuanxinxi.com/dicehead/221.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('222', 'Mr.Tang', 'http://www.zhongchuanxinxi.com/dicehead/222.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('223', '熊猫宇', 'http://www.zhongchuanxinxi.com/dicehead/223.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('224', '离风', 'http://www.zhongchuanxinxi.com/dicehead/224.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('225', '囧囧在努力', 'http://www.zhongchuanxinxi.com/dicehead/225.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('226', 'choreman', 'http://www.zhongchuanxinxi.com/dicehead/226.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('227', '西早晨', 'http://www.zhongchuanxinxi.com/dicehead/227.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('228', '啊栋', 'http://www.zhongchuanxinxi.com/dicehead/228.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('229', '虫卟知...', 'http://www.zhongchuanxinxi.com/dicehead/229.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('230', '李勋', 'http://www.zhongchuanxinxi.com/dicehead/230.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('231', '啊奇', 'http://www.zhongchuanxinxi.com/dicehead/231.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('232', '大年', 'http://www.zhongchuanxinxi.com/dicehead/232.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('233', '蓝色的风', 'http://www.zhongchuanxinxi.com/dicehead/233.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('234', '韩', 'http://www.zhongchuanxinxi.com/dicehead/234.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('235', '风笛', 'http://www.zhongchuanxinxi.com/dicehead/235.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('236', '孤星泪', 'http://www.zhongchuanxinxi.com/dicehead/236.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('237', '苏思源', 'http://www.zhongchuanxinxi.com/dicehead/237.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('238', '风。逐沙', 'http://www.zhongchuanxinxi.com/dicehead/238.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('239', 'Rambo', 'http://www.zhongchuanxinxi.com/dicehead/239.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('240', 'Wyman', 'http://www.zhongchuanxinxi.com/dicehead/240.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('241', 'The One', 'http://www.zhongchuanxinxi.com/dicehead/241.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('242', 'Jason', 'http://www.zhongchuanxinxi.com/dicehead/242.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('243', '徐梦阳', 'http://www.zhongchuanxinxi.com/dicehead/243.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('244', '啊蚊', 'http://www.zhongchuanxinxi.com/dicehead/244.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('245', '天邊有多遠', 'http://www.zhongchuanxinxi.com/dicehead/245.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('246', '码农', 'http://www.zhongchuanxinxi.com/dicehead/246.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('247', '徐梦阳', 'http://www.zhongchuanxinxi.com/dicehead/247.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('248', '萌面赵先生', 'http://www.zhongchuanxinxi.com/dicehead/248.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('249', '时光倒回20年', 'http://www.zhongchuanxinxi.com/dicehead/249.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('250', 'Adugen', 'http://www.zhongchuanxinxi.com/dicehead/250.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('251', '耀叔', 'http://www.zhongchuanxinxi.com/dicehead/251.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('252', '大雄', 'http://www.zhongchuanxinxi.com/dicehead/252.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('253', '欧子睿', 'http://www.zhongchuanxinxi.com/dicehead/253.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('254', '林则浩', 'http://www.zhongchuanxinxi.com/dicehead/254.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('255', '吴超', 'http://www.zhongchuanxinxi.com/dicehead/255.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('256', '周建斌', 'http://www.zhongchuanxinxi.com/dicehead/256.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('257', '清少', 'http://www.zhongchuanxinxi.com/dicehead/257.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('258', '陈皓', 'http://www.zhongchuanxinxi.com/dicehead/258.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('259', '洲', 'http://www.zhongchuanxinxi.com/dicehead/259.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('260', '然', 'http://www.zhongchuanxinxi.com/dicehead/260.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('261', '周荣', 'http://www.zhongchuanxinxi.com/dicehead/261.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('262', '谢涛', 'http://www.zhongchuanxinxi.com/dicehead/262.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('263', '暴走的萝卜', 'http://www.zhongchuanxinxi.com/dicehead/263.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('264', '韦韩', 'http://www.zhongchuanxinxi.com/dicehead/264.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('265', 'Hill', 'http://www.zhongchuanxinxi.com/dicehead/265.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('266', '老何', 'http://www.zhongchuanxinxi.com/dicehead/266.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('267', 'TXJ', 'http://www.zhongchuanxinxi.com/dicehead/267.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('268', '蒋大少', 'http://www.zhongchuanxinxi.com/dicehead/268.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('269', '布丁云', 'http://www.zhongchuanxinxi.com/dicehead/269.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('270', '土豆zone', 'http://www.zhongchuanxinxi.com/dicehead/270.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('271', '张奕', 'http://www.zhongchuanxinxi.com/dicehead/271.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('272', '陈成杰', 'http://www.zhongchuanxinxi.com/dicehead/272.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('273', '刘俊', 'http://www.zhongchuanxinxi.com/dicehead/273.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('274', '李煜', 'http://www.zhongchuanxinxi.com/dicehead/274.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('275', '赵小胖', 'http://www.zhongchuanxinxi.com/dicehead/275.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('276', '溪源泽', 'http://www.zhongchuanxinxi.com/dicehead/276.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('277', '李鹏彦', 'http://www.zhongchuanxinxi.com/dicehead/277.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('278', '肥恒', 'http://www.zhongchuanxinxi.com/dicehead/278.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('279', '林斌', 'http://www.zhongchuanxinxi.com/dicehead/279.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('280', '霍勇', 'http://www.zhongchuanxinxi.com/dicehead/280.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('281', 'Canis Lupus', 'http://www.zhongchuanxinxi.com/dicehead/281.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('282', '南', 'http://www.zhongchuanxinxi.com/dicehead/282.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('283', 'DLX', 'http://www.zhongchuanxinxi.com/dicehead/283.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('284', '成景嘉', 'http://www.zhongchuanxinxi.com/dicehead/284.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('285', 'jambin', 'http://www.zhongchuanxinxi.com/dicehead/285.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('286', '欧阳', 'http://www.zhongchuanxinxi.com/dicehead/286.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('287', 'Mhzheng', 'http://www.zhongchuanxinxi.com/dicehead/287.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('288', '小周', 'http://www.zhongchuanxinxi.com/dicehead/288.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('289', '找不到北', 'http://www.zhongchuanxinxi.com/dicehead/289.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('290', '尹小平', 'http://www.zhongchuanxinxi.com/dicehead/290.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('291', '江子龙', 'http://www.zhongchuanxinxi.com/dicehead/291.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('292', '叶峰', 'http://www.zhongchuanxinxi.com/dicehead/292.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('293', '谭源', 'http://www.zhongchuanxinxi.com/dicehead/293.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('294', 'Cole', 'http://www.zhongchuanxinxi.com/dicehead/294.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('295', '肖仙森', 'http://www.zhongchuanxinxi.com/dicehead/295.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('296', '罗伟良', 'http://www.zhongchuanxinxi.com/dicehead/296.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('297', '成仔', 'http://www.zhongchuanxinxi.com/dicehead/297.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('298', 'Z木衫', 'http://www.zhongchuanxinxi.com/dicehead/298.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('299', '冯志威', 'http://www.zhongchuanxinxi.com/dicehead/299.jpg', '0', '0', '0', '0', '0');
INSERT INTO `robot` VALUES ('300', '天翔', 'http://www.zhongchuanxinxi.com/dicehead/300.jpg', '0', '0', '0', '0', '0');

-- ----------------------------
-- Table structure for `role`
-- ----------------------------
DROP TABLE IF EXISTS `role`;
CREATE TABLE `role` (
  `role_id` int(11) unsigned NOT NULL,
  `sid` varchar(255) DEFAULT NULL,
  `openid` varchar(30) NOT NULL DEFAULT '',
  `parent_id` int(11) NOT NULL DEFAULT '0' COMMENT '爸爸ID',
  `nickname` varchar(50) DEFAULT NULL,
  `icon` varchar(255) DEFAULT NULL,
  `created_at` int(10) DEFAULT NULL,
  `charm` int(11) NOT NULL DEFAULT '0' COMMENT '魅力值',
  `balance` bigint(20) NOT NULL DEFAULT '0',
  `deposit` bigint(20) NOT NULL DEFAULT '0',
  `withdraw` bigint(20) NOT NULL DEFAULT '0',
  `gift_val` int(10) unsigned NOT NULL DEFAULT '0',
  `phone` varchar(20) DEFAULT NULL,
  `login_time` int(10) NOT NULL DEFAULT '0',
  `agent_id` int(11) NOT NULL DEFAULT '0',
  `p` bigint(20) NOT NULL DEFAULT '0',
  `w` bigint(20) NOT NULL DEFAULT '0',
  `vip` int(11) NOT NULL DEFAULT '0' COMMENT '分销等级',
  `login_gift` int(11) NOT NULL DEFAULT '0',
  `last_login` int(11) NOT NULL DEFAULT '0',
  `chip` bigint(20) unsigned NOT NULL DEFAULT '0',
  `kick_time` int(10) unsigned NOT NULL DEFAULT '0',
  `red_bag` int(10) unsigned NOT NULL DEFAULT '0',
  `off_time` int(32) DEFAULT '0' COMMENT '离线时间',
  `red_openid` varchar(40) DEFAULT NULL COMMENT '企业付款openid',
  `pay_openid` varchar(40) DEFAULT NULL COMMENT '支付openid',
  PRIMARY KEY (`role_id`),
  KEY `parent_id` (`parent_id`),
  KEY `agent_id` (`agent_id`),
  KEY `created_at` (`created_at`),
  KEY `openid` (`openid`) USING BTREE,
  KEY `red_openid` (`red_openid`),
  KEY `pay_openid` (`pay_openid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of role
-- ----------------------------
INSERT INTO `role` VALUES ('20001', '', '30', '0', '?????จุ๊บ', null, '1607415691', '0', '0', '0', '0', '0', '', '0', '33', '0', '0', '0', '0', '0', '0', '0', '0', '0', '', '');

-- ----------------------------
-- Table structure for `role_bill`
-- ----------------------------
DROP TABLE IF EXISTS `role_bill`;
CREATE TABLE `role_bill` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `role_id` int(10) unsigned NOT NULL,
  `luck_id` int(10) unsigned NOT NULL COMMENT '期数',
  `time` int(10) unsigned NOT NULL COMMENT '时间',
  `info` text NOT NULL,
  `bet_val` int(10) NOT NULL DEFAULT '0',
  `win_val` int(10) NOT NULL DEFAULT '0',
  `type` int(11) NOT NULL COMMENT '游戏类型',
  PRIMARY KEY (`id`),
  KEY `index_name` (`time`,`bet_val`,`win_val`,`luck_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of role_bill
-- ----------------------------

-- ----------------------------
-- Table structure for `role_log`
-- ----------------------------
DROP TABLE IF EXISTS `role_log`;
CREATE TABLE `role_log` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `role_id` int(10) unsigned NOT NULL,
  `time` int(10) unsigned NOT NULL COMMENT '时间',
  `amount` int(11) NOT NULL COMMENT '变化值',
  `balance` bigint(20) unsigned NOT NULL,
  `type` int(11) unsigned NOT NULL COMMENT '金豆变化类型',
  `recharge_id` text,
  PRIMARY KEY (`id`),
  KEY `role_id` (`role_id`,`time`,`amount`,`type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of role_log
-- ----------------------------

-- ----------------------------
-- Table structure for `room`
-- ----------------------------
DROP TABLE IF EXISTS `room`;
CREATE TABLE `room` (
  `room_id` int(11) NOT NULL,
  `p` int(11) NOT NULL DEFAULT '0',
  `w` int(11) NOT NULL DEFAULT '0',
  `type` int(11) NOT NULL DEFAULT '1',
  `bet_time` int(11) NOT NULL DEFAULT '30',
  `open_time` int(11) NOT NULL DEFAULT '25',
  `bet_val_one` int(11) NOT NULL DEFAULT '100' COMMENT '下注档位1',
  `bet_val_two` int(11) NOT NULL DEFAULT '500',
  `bet_val_three` int(11) NOT NULL DEFAULT '1000',
  `bet_val_four` int(11) NOT NULL DEFAULT '2000',
  `bet_val_five` int(11) NOT NULL DEFAULT '5000',
  `bet_val_six` int(11) NOT NULL DEFAULT '10000',
  `odds` int(11) NOT NULL DEFAULT '1' COMMENT '赔率',
  `twelve_odds` float DEFAULT '12' COMMENT '12生肖赔率',
  `five_odds` float NOT NULL DEFAULT '5' COMMENT '5福赔率',
  `four_odds` float NOT NULL DEFAULT '4' COMMENT '4门赔率',
  `two_odds` float NOT NULL DEFAULT '2' COMMENT '金童玉女赔率',
  `one_odds` int(11) NOT NULL DEFAULT '120' COMMENT '灾害险赔率',
  `small_odds` int(11) NOT NULL DEFAULT '24' COMMENT '小富贵赔率',
  `big_odds` int(11) NOT NULL DEFAULT '120' COMMENT '大富贵赔率',
  `good_luck_name` varchar(10) NOT NULL DEFAULT 'ox' COMMENT '幸运12生肖名字',
  `good_luck_val` int(11) NOT NULL DEFAULT '15' COMMENT '幸运12生肖加奖',
  `small_luck_name` varchar(40) NOT NULL DEFAULT '[boy,pig]',
  `big_luck_name` varchar(40) NOT NULL DEFAULT '[girl,fortune,ox]',
  `big_luck_time` int(11) NOT NULL DEFAULT '86400' COMMENT '更新大富贵时间',
  `small_luck_time` int(11) NOT NULL DEFAULT '3600' COMMENT '更新小富贵时间',
  `farm_growth` varchar(999) NOT NULL DEFAULT '[]' COMMENT '农场大亨农作物生长',
  `gift_val` int(11) NOT NULL DEFAULT '0' COMMENT '每日一猜送分',
  `daily_guess_roles` varchar(999) NOT NULL DEFAULT '[]' COMMENT '每日一猜的玩家ID',
  `first_free_doll` int(10) unsigned NOT NULL DEFAULT '0',
  `free_doll` int(10) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`room_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of room
-- ----------------------------
INSERT INTO `room` VALUES ('1', '0', '0', '1', '30', '20', '100', '1000', '5000', '10000', '20000', '50000', '1', '12', '5', '4', '2', '120', '24', '120', 'ox', '15', '[boy,pig]', '[girl,fortune,ox]', '86400', '3600', '[]', '0', '[]', '0', '0');
INSERT INTO `room` VALUES ('2', '0', '0', '2', '30', '25', '100', '1000', '5000', '10000', '20000', '50000', '1', '12', '5', '4', '2', '120', '24', '120', 'ox', '15', '[boy,pig]', '[girl,fortune,ox]', '86400', '3600', '[]', '0', '[]', '0', '0');
INSERT INTO `room` VALUES ('3', '0', '0', '1', '30', '25', '100', '500', '1000', '2000', '5000', '10000', '1', '12', '5', '4', '2', '120', '24', '120', 'ox', '15', '[boy,pig]', '[girl,fortune,ox]', '86400', '3600', '[]', '0', '', '0', '0');
INSERT INTO `room` VALUES ('4', '0', '0', '4', '30', '25', '100', '500', '1000', '2000', '5000', '10000', '1', '12', '5', '4', '2', '120', '24', '120', 'ox', '15', '[boy,pig]', '[girl,fortune,ox]', '86400', '3600', '[]', '0', '[]', '50', '100');
INSERT INTO `room` VALUES ('5', '0', '0', '5', '30', '25', '100', '500', '1000', '2000', '5000', '10000', '1', '12', '5', '4', '2', '120', '24', '120', 'ox', '15', '[boy,pig]', '[girl,fortune,ox]', '86400', '3600', '[]', '0', '[]', '0', '0');
INSERT INTO `room` VALUES ('6', '0', '0', '6', '20', '25', '100', '500', '1000', '2000', '5000', '10000', '1', '12', '5', '4', '2', '120', '24', '120', 'ox', '15', '[boy,pig]', '[girl,fortune,ox]', '86400', '3600', '[]', '0', '[]', '0', '0');
INSERT INTO `room` VALUES ('7', '0', '0', '7', '30', '25', '100', '500', '1000', '2000', '5000', '10000', '1', '12', '5', '4', '2', '120', '24', '120', 'ox', '15', '[boy,pig]', '[girl,fortune,ox]', '86400', '3600', '[]', '0', '[]', '0', '0');
INSERT INTO `room` VALUES ('8', '0', '0', '8', '30', '25', '100', '500', '1000', '2000', '5000', '10000', '1', '12', '5', '4', '2', '120', '24', '120', 'ox', '15', '[boy,pig]', '[girl,fortune,ox]', '86400', '3600', '[]', '0', '[]', '0', '0');
INSERT INTO `room` VALUES ('10', '0', '0', '10', '30', '25', '100', '500', '1000', '2000', '5000', '10000', '1', '0', '0', '0', '0', '0', '0', '0', 'ox', '0', '[]', '[]', '0', '0', '[]', '0', '[]', '0', '0');
INSERT INTO `room` VALUES ('11', '0', '0', '11', '30', '25', '100', '500', '1000', '2000', '5000', '10000', '1', '0', '0', '0', '0', '0', '0', '0', 'ox', '0', '[boy,pig]', '[girl,fortune,ox]', '0', '0', '[]', '0', '[]', '0', '0');

-- ----------------------------
-- Table structure for `setting`
-- ----------------------------
DROP TABLE IF EXISTS `setting`;
CREATE TABLE `setting` (
  `id` int(11) NOT NULL,
  `phone` varchar(40) NOT NULL,
  `qq` varchar(40) NOT NULL,
  `wx` varchar(40) NOT NULL,
  `type` int(11) NOT NULL,
  `luck_phone` varchar(11) DEFAULT NULL,
  `luck_qq` varchar(12) DEFAULT NULL,
  `luck_wx` varchar(40) DEFAULT NULL,
  `farm_phone` varchar(11) DEFAULT NULL,
  `farm_qq` varchar(12) DEFAULT NULL,
  `farm_wx` varchar(40) DEFAULT NULL,
  `free_state` int(11) NOT NULL DEFAULT '0' COMMENT '免费赠送夹娃娃次数开关0 ->关,1->开',
  `free_times` int(11) NOT NULL DEFAULT '0' COMMENT '免费赠送夹娃娃次数',
  `free_val` int(11) NOT NULL DEFAULT '0' COMMENT '首次免费夹娃娃1000',
  `luck_state` int(10) unsigned NOT NULL DEFAULT '0',
  `farm_state` int(10) unsigned NOT NULL DEFAULT '0',
  `phone_limit` int(10) unsigned NOT NULL DEFAULT '10',
  `doll_level` varchar(99) NOT NULL DEFAULT '[0,1,2,3,4,5,6]',
  `doll_limit` int(10) unsigned NOT NULL DEFAULT '100',
  `balance_limit` int(10) unsigned NOT NULL DEFAULT '100000',
  `all_doll_limit` int(10) unsigned NOT NULL DEFAULT '0',
  `all_balance_limit` int(10) unsigned NOT NULL DEFAULT '0',
  `deposit_state` int(10) unsigned NOT NULL DEFAULT '0',
  `d1` int(10) unsigned NOT NULL DEFAULT '0',
  `d2` int(10) unsigned NOT NULL DEFAULT '0',
  `d3` int(10) unsigned NOT NULL DEFAULT '0',
  `d4` int(10) unsigned NOT NULL DEFAULT '0',
  `d5` int(10) unsigned NOT NULL DEFAULT '0',
  `d6` int(10) unsigned NOT NULL DEFAULT '0',
  `d7` int(10) unsigned NOT NULL DEFAULT '0',
  `doll_withdraw` int(10) unsigned NOT NULL DEFAULT '0',
  `feedback_state` int(10) unsigned NOT NULL DEFAULT '1',
  `deposit_condition` int(10) unsigned NOT NULL DEFAULT '0',
  `withdraw_condition` int(10) unsigned NOT NULL DEFAULT '0',
  `agent_percent` int(10) unsigned NOT NULL DEFAULT '0',
  `account_time` int(10) unsigned NOT NULL DEFAULT '0',
  `doll_state` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '0关1开',
  `red_bag_state` int(10) unsigned NOT NULL DEFAULT '0',
  `role_limit` int(10) unsigned NOT NULL DEFAULT '1',
  `wxpublic` int(11) unsigned DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of setting
-- ----------------------------
INSERT INTO `setting` VALUES ('1', '1', '2', '3', '1', '4', '5', '6', '7', '8', '9', '1', '3', '188', '0', '0', '20', '[0,1,2,3,4,5,6]', '1000', '200000', '1000', '200000', '0', '0', '0', '0', '400', '500', '600', '700', '2000', '1', '300000000', '80', '600', '30', '1', '0', '5', '0');

-- ----------------------------
-- Table structure for `share_log`
-- ----------------------------
DROP TABLE IF EXISTS `share_log`;
CREATE TABLE `share_log` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `role_id` int(11) NOT NULL,
  `name` text NOT NULL,
  `icon` text NOT NULL,
  `item_id` int(11) NOT NULL,
  `item_name` text NOT NULL,
  `words` longtext,
  `picture` text,
  `recommend` int(11) NOT NULL DEFAULT '0',
  `time` int(11) NOT NULL,
  `share_val` int(11) NOT NULL DEFAULT '0' COMMENT '晒单获得金豆',
  `do_like` int(11) NOT NULL DEFAULT '0',
  `vip` int(11) NOT NULL DEFAULT '0',
  `reward_id` int(11) DEFAULT NULL COMMENT '中奖记录的ID',
  `prize_id` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `time` (`time`),
  KEY `role_id` (`role_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of share_log
-- ----------------------------

-- ----------------------------
-- Table structure for `statistics`
-- ----------------------------
DROP TABLE IF EXISTS `statistics`;
CREATE TABLE `statistics` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `room_id` int(11) NOT NULL DEFAULT '0',
  `all_number` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '夹娃娃总次数',
  `hit_number` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '夹中次数',
  `not_hit` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '未夹中次数',
  `air_boll` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '夹空次数',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=8 DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of statistics
-- ----------------------------
INSERT INTO `statistics` VALUES ('1', '1', '0', '0', '0', '0');
INSERT INTO `statistics` VALUES ('2', '2', '0', '0', '0', '0');
INSERT INTO `statistics` VALUES ('3', '3', '0', '0', '0', '0');
INSERT INTO `statistics` VALUES ('4', '4', '0', '0', '0', '0');
INSERT INTO `statistics` VALUES ('5', '5', '0', '0', '0', '0');
INSERT INTO `statistics` VALUES ('6', '6', '0', '0', '0', '0');
INSERT INTO `statistics` VALUES ('7', '0', '0', '0', '0', '0');

-- ----------------------------
-- Table structure for `stock_log`
-- ----------------------------
DROP TABLE IF EXISTS `stock_log`;
CREATE TABLE `stock_log` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `role_id` int(11) NOT NULL DEFAULT '0',
  `stage` int(11) NOT NULL DEFAULT '0',
  `round` bigint(20) unsigned NOT NULL DEFAULT '0',
  `time` int(11) NOT NULL DEFAULT '0',
  `bet_val` int(11) NOT NULL DEFAULT '0',
  `win_val` int(11) NOT NULL DEFAULT '0',
  `point` int(11) NOT NULL DEFAULT '0',
  `lines` text,
  `detail` longtext,
  `type` tinyint(2) NOT NULL DEFAULT '2' COMMENT '1=真实,2=虚拟',
  PRIMARY KEY (`id`),
  KEY `role_id` (`role_id`),
  KEY `time` (`stage`),
  KEY `round` (`round`),
  KEY `bet_val` (`bet_val`),
  KEY `win_val` (`win_val`),
  KEY `type` (`type`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of stock_log
-- ----------------------------

-- ----------------------------
-- Table structure for `task`
-- ----------------------------
DROP TABLE IF EXISTS `task`;
CREATE TABLE `task` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `type` int(10) unsigned NOT NULL COMMENT '奖励类型',
  `game_type` int(10) unsigned NOT NULL COMMENT '游戏类型',
  `condition` varchar(40) NOT NULL COMMENT '条件',
  `val` varchar(40) NOT NULL COMMENT '奖励值',
  `state` int(10) unsigned NOT NULL COMMENT '任务状态',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=11 DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of task
-- ----------------------------
INSERT INTO `task` VALUES ('1', '2', '1', '[5,10,20]', '1', '1');
INSERT INTO `task` VALUES ('2', '1', '1', '[3,8,15]', '[68,98,188]', '1');
INSERT INTO `task` VALUES ('3', '2', '2', '[5,10,20]', '1', '1');
INSERT INTO `task` VALUES ('4', '1', '2', '[3,8,15]', '[68,98,188]', '1');
INSERT INTO `task` VALUES ('5', '1', '5', '[3,8,15]', '[68,98,188]', '1');
INSERT INTO `task` VALUES ('6', '2', '5', '[8,15,30]', '1', '1');
INSERT INTO `task` VALUES ('7', '2', '11', '[5,10,20]', '1', '1');
INSERT INTO `task` VALUES ('8', '1', '11', '[3,8,15]', '[68,98,188]', '1');
INSERT INTO `task` VALUES ('9', '2', '7', '[8,20,50]', '1', '1');
INSERT INTO `task` VALUES ('10', '1', '7', '[3,8,15]', '[68,98,188]', '1');

-- ----------------------------
-- Table structure for `task_log`
-- ----------------------------
DROP TABLE IF EXISTS `task_log`;
CREATE TABLE `task_log` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `role_id` int(10) unsigned NOT NULL,
  `luck` int(10) unsigned NOT NULL DEFAULT '0',
  `farm` int(10) unsigned NOT NULL DEFAULT '0',
  `fish` int(10) unsigned NOT NULL DEFAULT '0',
  `stock` int(10) unsigned DEFAULT '0',
  `time` int(10) unsigned NOT NULL,
  `state` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '0未发奖1已发奖',
  `zoo` int(11) unsigned DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `role_id` (`role_id`),
  KEY `time` (`time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of task_log
-- ----------------------------

-- ----------------------------
-- Table structure for `treasure_log`
-- ----------------------------
DROP TABLE IF EXISTS `treasure_log`;
CREATE TABLE `treasure_log` (
  `id` int(11) NOT NULL DEFAULT '0' COMMENT '唯一id',
  `gold` int(11) DEFAULT NULL COMMENT '总押注的钱',
  `win` int(11) DEFAULT NULL COMMENT '总产出的钱',
  `jack_prot` int(11) DEFAULT NULL COMMENT '奖金池',
  `time` int(11) DEFAULT NULL COMMENT '时间',
  `result` varchar(255) DEFAULT NULL COMMENT '结果',
  `profit` int(11) DEFAULT NULL COMMENT '本期自己盈利',
  `role_num` int(11) DEFAULT NULL COMMENT '本期玩家人数',
  PRIMARY KEY (`id`),
  KEY `index` (`gold`,`time`,`profit`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of treasure_log
-- ----------------------------

-- ----------------------------
-- Table structure for `treasure_role`
-- ----------------------------
DROP TABLE IF EXISTS `treasure_role`;
CREATE TABLE `treasure_role` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '自增id',
  `role_id` int(11) NOT NULL DEFAULT '0' COMMENT '人物id',
  `multiple` int(11) DEFAULT NULL COMMENT '倍数',
  `gold` int(11) DEFAULT NULL COMMENT '总押注的钱',
  `win` int(11) DEFAULT NULL COMMENT '总产出的钱',
  `odds` varchar(255) DEFAULT NULL COMMENT '押注列表',
  `time` int(11) DEFAULT NULL COMMENT '押注时间',
  `num` int(11) DEFAULT NULL COMMENT '期数',
  `result` varchar(255) DEFAULT NULL COMMENT '结果',
  `all_time` int(11) DEFAULT NULL COMMENT '开奖时间',
  PRIMARY KEY (`id`),
  KEY `index` (`role_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of treasure_role
-- ----------------------------

-- ----------------------------
-- Table structure for `wft`
-- ----------------------------
DROP TABLE IF EXISTS `wft`;
CREATE TABLE `wft` (
  `id` varchar(40) NOT NULL DEFAULT '',
  `role_id` int(11) unsigned NOT NULL,
  `gold` int(11) unsigned NOT NULL,
  `gift` int(10) unsigned NOT NULL DEFAULT '0',
  `cny` int(10) unsigned NOT NULL COMMENT '单位分',
  `time` int(10) unsigned NOT NULL,
  `status` tinyint(1) unsigned NOT NULL DEFAULT '0',
  `agent_id` int(10) unsigned NOT NULL DEFAULT '0',
  `emapay_id` varchar(40) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `index_name` (`role_id`,`time`,`agent_id`,`status`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of wft
-- ----------------------------
