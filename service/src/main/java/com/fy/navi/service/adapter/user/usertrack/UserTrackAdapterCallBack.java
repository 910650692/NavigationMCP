package com.fy.navi.service.adapter.user.usertrack;

import com.fy.navi.service.define.user.usertrack.GpsTrackDepthBean;


public interface UserTrackAdapterCallBack {

    /**
     * 开启Gps轨迹生成的回调通知
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @param n32SuccessTag 状态
     * -1 失败
     * 0 成功：新建轨迹文件进行打点
     * 1 成功：在已存在的轨迹文件继续追加打点
     */
    void onStartGpsTrack(int n32SuccessTag, String psSavePath, String psFileName);

    /**
     * 关闭Gps轨迹生成的回调通知
     * @param n32SuccessTag 状态
     * -1 失败
     * 0 成功：新建轨迹文件进行打点
     * 1 成功：在已存在的轨迹文件继续追加打点
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @param depInfo 轨迹文件信息
     */
    void onCloseGpsTrack(int n32SuccessTag, String psSavePath, String psFileName, GpsTrackDepthBean depInfo);

    /**
     * 获取Gps轨迹文件深度信息的回调通知
     * @param n32SuccessTag 状态
     * -1 失败
     * 0 成功：新建轨迹文件进行打点
     * 1 成功：在已存在的轨迹文件继续追加打点
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @param depInfo 轨迹文件信息
     */
    void onGpsTrackDepInfo(int n32SuccessTag, String psSavePath, String psFileName, GpsTrackDepthBean depInfo);

    /**
     * 获取轨迹数据同步回调通知
     * @param eventType 同步SDK回调事件类型
     * @param exCode 同步SDK返回值
     */
    void notify(int eventType,  int exCode);
}
