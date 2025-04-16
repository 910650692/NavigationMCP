package com.fy.navi.service.adapter.navi;

import com.fy.navi.service.define.cruise.CruiseParamEntity;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.navi.NaviParamEntity;
import com.fy.navi.service.define.navi.NaviStartType;
import com.fy.navi.service.define.navi.NaviViaEntity;

import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/5
 */
public interface INaviApi {

    /**
     * 初始化导航服务
     */
    void initNaviService();

    /**
     * 注册观察者
     *
     * @param key              key
     * @param guidanceObserver guidanceObserver
     */
    void registerObserver(String key, GuidanceObserver guidanceObserver);

    /**
     * 开始导航
     *
     * @param isSimple isSimple
     * @return boolean
     */
    boolean startNavigation(NaviStartType isSimple);

    /**
     * 停止导航
     *
     * @return boolean
     */
    boolean stopNavigation();

    /**
     * 设置导航路线
     *
     * @param routeIndex          routeIndex
     * @param routeLineLayerParam routeLineLayerParam
     */
    void setNaviPath(int routeIndex, RouteLineLayerParam routeLineLayerParam);

    /**
     * 更新导航路线
     *
     * @param routeIndex          routeIndex
     * @param routeLineLayerParam routeLineLayerParam
     */
    void updateNaviPath(int routeIndex, RouteLineLayerParam routeLineLayerParam);

    /**
     * 注销观察者
     *
     * @param key key
     */
    void unregisterObserver(String key);

    /**
     * 销毁导航服务
     */
    void unInitNaviService();

    /**
     * 主动获取剩余路线上的SAPA信息
     *
     * @param isFindRemainPath 是否查找剩余路径
     * @return long
     */
    long obtainSAPAInfo(boolean isFindRemainPath);

    /**
     * 选择主路id
     *
     * @param pathID pathID
     */
    void selectMainPathID(long pathID);

    /**
     * 设置巡航参数
     *
     * @param cruiseParamEntity cruiseParamEntity
     */
    void setCruiseParam(CruiseParamEntity cruiseParamEntity);

    /**
     * 更新导航参数
     *
     * @param naviParamEntity naviParamEntity
     */
    void updateGuideParam(NaviParamEntity naviParamEntity);

    /**
     * 主动查询前方路况
     *
     * @param requestId requestId
     */
    void playTRManualExt(int requestId);

    /**
     * 查询道路信息
     *
     * @param segmentIdx segmentIdx
     * @param linkIdx    linkIdx
     */
    void queryAppointLanesInfo(int segmentIdx, int linkIdx);

    /**
     * 获取路线上的隧道长度
     */
    void getTunnelLength();

    /**
     * 更新电量信息
     */
    void updateBatteryInfo();

    /***
     * 此接口属于动态获取
     * @return 获取途径点信息
     */
    List<NaviViaEntity> getAllViaPoints();

    /**
     * 暂停
     */
    void pauseNavi();

    /**
     * 开始
     */
    void resumeNavi();

    /**
     * 设置模拟导航速度
     */
    void setSimulationSpeed(int simulationSpeed);
}
