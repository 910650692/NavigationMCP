package com.sgm.navi.service.adapter.engine;


import com.sgm.navi.service.define.engine.GaodeLogLevel;
import com.sgm.navi.service.define.map.MapType;

/**
 * TODO 说明
 * @author lvww
 * @version $Revision.2024/11/26$
 */
public interface IEngineApi {

    void loadLibrary();

    /**
     * 添加初始化观察者
     * @param observer EngineObserver
     */
    void addInitEnginObserver(EngineObserver observer);

    /**
     * 检查是否超期,只有应用内部会在接近超期时进行弹窗提醒，外部启动引擎不做此检查
     */
    void checkSdkLimit();

    /**
     * 始化BaseLib
     */
    void initBaseLibs();

    /**
     * 初始化BLLib
     */
    void initBL();

    /**
     * 获取引擎id
     * @param mapId mapId
     * @return int mapId
     */
    int engineID(MapType mapId);

    /**
     * 获取鹰眼引擎id
     * @param mapId MapType
     * @return 鹰眼引擎id
     */
    int eagleEyeEngineID(MapType mapId);

    /**
     * 获取EGLDeviceID
     * @param mapId MapType
     * @return EGLDeviceID
     */
    int mapDeviceID(MapType mapId);

    /**
     * 切换log级别
     * @param logLevel logLevel
     */
    void switchLog(GaodeLogLevel logLevel);

    /**
     * 反初始化引擎
     */
    void unInit();

    /**
     * 获取sdk版本
     * @return sdk版本
     */
    String getSdkVersion();

    /**
     * 获取引擎版本
     * @return 引擎版本
     */
    String getEngineVersion();

    /**
     * 获取style_bl路径
     * @param mapTypeId MapType
     * @return 路径字符串
     */
    String styleBlPath(MapType mapTypeId);

    /**
     * 获取SDK的渠道名
     * @return Chanel Name
     */
    String getChanelName();

    boolean engineIsInit();
}
