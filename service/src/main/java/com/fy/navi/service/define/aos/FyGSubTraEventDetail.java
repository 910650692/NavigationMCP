package com.fy.navi.service.define.aos;

/**
 * Author: QiuYaWei
 * Date: 2025/2/27
 * Description: [交通事件-子事件]
 */

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class FyGSubTraEventDetail implements Serializable {
    public int criticism; // 事件被踩到次数
    public int layer;
    public String id;//事件id
    public String lastupdate;// 最后更新时间 utc秒
    public String infoenddate; // 有情报时间的（hasdetail不为0），结束日期，时间
    public String title;// 事件描述信息（老版本的描述，title字段的内容 是由起止时间、车道等信息加desc字段拼接而成的，
    public int source;// 事件源id
    public int layertag;
    public String nick;// 用户昵称
    public String head;  //   标题，没有为空字符串，用以客户端显示新旧卡片，来自发布服务的 headline字段 透传
    public String infotimeseg;// 有情报时间（hasdetail不为0）
    public int audiolen;// 音频播放的长度（单位为秒）
    public String address;//事件位置描述（逆地理编码）
    public String desc;//详细描述。来自发布服务的 newdesc字段 透传
    public String lane;//占据车道信息，没有为空字符串（占据左侧、中间、右侧车道）
    public String infostartdate;//有情报时间的（hasdetail不为0），开始日期，时间 2016年7月20日 19：30
    public int official;//0：官方 1：权威 2：其他
    public String expirytime;//事件过期时间 utc秒
    public String picurl;//事件所带图片的url
    public int praise;//事件被赞的次数
    public String audio;//事件所带音频的url
    public String createtime;//事件上报时间 （或开始事件）utc秒
    public double fLon;//经度，本地获取
    public double fLat;//纬度，本地获取
    public String iconstyle;//运营事件样式（如果事件不在运营时间有效期内，该字段为空，“”）
    public String eventname;// 事件名称
    public String engBrief;//英文描述
    public String avatar;//用户头像
    public String labelDesc;//标签描述信息
    public FyGTrifficSocolPicture socol_picture; // 后视境自动采集图片信息
    public FyGSubTraEventDetail() {
    }
}
