package com.fy.navi.service.define.aos;


import com.fy.navi.service.R;

/**
 * Author: QiuYaWei
 * Date: 2025/3/13
 * Description: [交通事件类型]
 */
public class TrafficType {
    // 故障
    public static final int TRAFFIC_ACCIDENT = 11010;
    // 交通事故-车祸
    public static final int TRAFFIC_COLLISION = 11011;
    // 封路
    public static final int TRAFFIC_STOP_PASS = 11050;
    // 交通管制
    public static final int TRAFFIC_CONTROL = 11031;
    // 施工
    public static final int TRAFFIC_ROAD_REPAIR = 11040;
    // 公告
    public static final int TRAFFIC_PUBLIC = 11070;
    // 拥堵
    public static final int TRAFFIC_CROWD = 11021;

    /***
     * 根据交通事件类型获取图标
     * @return
     */
    public static int getIconByTrafficType(int type) {
        int icon = R.drawable.img_guanzhi_96;
        switch (type) {
            // TODO 暂时把故障划到事故类型
            case TRAFFIC_ACCIDENT, TRAFFIC_COLLISION -> {
                icon = R.drawable.img_shigu_96;
            }
            case TRAFFIC_STOP_PASS -> {
                icon = R.drawable.img_fenglu_96;
            }
            case TRAFFIC_CONTROL -> {
                icon = R.drawable.img_guanzhi_96;
            }
            case TRAFFIC_ROAD_REPAIR -> {
                icon = R.drawable.img_shigong_96;
            }
            case TRAFFIC_CROWD -> {
                icon = R.drawable.img_yongdu_96;
            }
        }
        return icon;
    }

    /***
     * 根据交通事件类型获取事故标题
     * @return
     */
    public static int getTitleByTrafficType(int type) {
        int title = R.string.traffic_crowd;
        switch (type) {
            // TODO 暂时把故障划到事故类型
            case TRAFFIC_ACCIDENT, TRAFFIC_COLLISION -> {
                title = R.string.traffic_collision;
            }
            case TRAFFIC_STOP_PASS -> {
                title = R.string.traffic_stop;
            }
            case TRAFFIC_CONTROL -> {
                title = R.string.traffic_control;
            }
            case TRAFFIC_ROAD_REPAIR -> {
                title = R.string.traffic_repair;
            }
            case TRAFFIC_CROWD -> {
                title = R.string.traffic_crowd;
            }
        }
        return title;
    }
}
