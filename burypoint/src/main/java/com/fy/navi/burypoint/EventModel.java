package com.fy.navi.burypoint;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/6
 */
public class EventModel {
    public static class Event{
        /*** 事件组 **/
        private String eventGroup;
        /*** 事件组类型**/
        private String eventType;
        /*** 事件名字 **/
        private String eventName;
        /*** 事件发生时间 **/
        private String eventTime;
        /*** 扩展数据 **/
        private String eventData;
    }

    public static class EventUserInfo{
        /*** 设备品牌 **/
        private String carBrand;
        /*** 设备类型 **/
        private String carModel;
        /*** 设备名称 **/
        private String carName;
        /*** 设备型号 **/
        private String carNum;
        /*** 设备UID **/
        private String carUid;
        /*** 设备软件版本 **/
        private String carNaviVersion;
        /*** 用户ID **/
        private String carUserId;
    }
}
