package com.fy.navi.service.adapter.mapdata;

import com.fy.navi.service.define.bean.AdminCodeBean;
import com.fy.navi.service.define.bean.AreaExtraInfoBean;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;

import java.util.ArrayList;

public interface IMapDataApi {

    /**
     * 初始化离线数据
     */
    void init();

    /**
     * 获取所有省份+城市信息
     * @return 返回所有离线数据信息
     */
    ArrayList<ProvDataInfo> getMapDataList();

    /**
     * 获取基础功能包数据
     * @return 返回基础包信息
     */
    CityDataInfo getCountryData();

    /**
     * 通过adCode获取城市信息
     * @param adCode
     * @return 返回城市信息
     */
    CityDataInfo getCityInfo(int adCode);

    /**
     * 获取区域扩展信息
     * @param adminCode
     * @return 返回区域扩展信息
     */
    AreaExtraInfoBean getAreaExtraInfo(AdminCodeBean adminCode);

    /**
     * 根据经纬度解析获取城市adCode
     * 若解析失败,则返回0
     * 调用该函数只需要获取MapDataService即可，不要求初始化成功
     * @param lon
     * @param lat
     * @return 返回adCode
     */
    int getAdCodeByLonLat(double lon, double lat);

    /**
     * 获取 已下载的 省份+城市 结构 信息
     * 该方法对外提供
     * @return 返回所有已下载的数据
     */
    ArrayList<ProvDataInfo> getAllDownLoadedList();

    /**
     * 通过关键字获取 adcode 列表
     * @param downLoadMode
     * @param strKey
     * @return 返回adCode列表
     */
    ArrayList<Integer> getAdCodeList(int downLoadMode, String strKey);

    /**
     * 通过搜索关键字获取行政区域adcode列表
     * @param strKey
     * @return 返回匹配关键字的信息
     */
    ArrayList<ProvDataInfo> searchAdCode(String strKey);

    /**
     * 通过搜索关键字获取已下载的行政区域adcode列表
     * 该方法对外提供使用
     *
     * @param strKey
     * @return 返回匹配到关键的已下载信息
     */
    ArrayList<ProvDataInfo> searchDownLoaded(String strKey);

    /**
     * 通过搜索城市关键字获取行政区域adcode列表
     * @param strKey
     * @return 返回对应的行政区域信息
     */
    int searchCityAdCode(String strKey);

    /**
     * 离线地图数据下载专用接口
     * 下载模式downLoadMode=DOWNLOAD_MODE_NET时，需要等【首次】RequestDataListCheck请求的观察者监听pObserver回调OnRequestDataListCheck后调用。
     * 下载模式downLoadMode=DOWNLOAD_MODE_USB时，需要每次等RequestDataListCheck请求的观察者监听pObserver回调OnRequestDataListCheck后调用。
     * @return 返回等待中、下载中、暂停、解压中、重试状态下的所有城市adCode列表
     */
    ArrayList<Integer> getWorkingQueueAdCodeList();

    /**
     * 获取下载中、更新中状态下的所有城市adCode列表
     * @return 返回下载中的信息
     */
    ArrayList<CityDataInfo> getWorkingList();

    /**
     * 获取已下载状态下的所有城市adCode列表
     * @return 返回已下载的信息
     */
    ArrayList<CityDataInfo> getWorkedList();

    /**
     * 通过adCode获取附近推荐城市信息
     * @param adCode
     * @return 返回附近推荐城市信息
     */
    ArrayList<CityDataInfo> getNearAdCodeList(int adCode);

    /**
     * 获取离线数据版本号
     * @param adCode  当前城市的行政编码
     * @return 返回离线数据版本信息
     */
    String getDataFileVersion(int adCode);

    /**
     * 设置当前城市的行政编码
     * @param adCode
     */
    void setCurrentCityAdCode(int adCode);

    /**
     * 中断离线数据检测
     * @param downloadMode
     */
    void abortDataListCheck(int downloadMode);

    /**
     * 离线数据下载操作
     * @param opType
     * @param adCodeDiyLst
     */
    void operate(int opType, ArrayList<Integer> adCodeDiyLst);

    /**
     * 注册离线信息回调
     * @param key
     * @param callBack
     */
    void registerCallBack(String key, MapDataAdapterCallBack callBack);

    /**
     * 反注册离线信息回调
     * @param key
     * @param callBack
     */
    void unRegisterCallback(String key, MapDataAdapterCallBack callBack);

    /**
     * 反初始化离线数据服务
     */
    void unInit();

    /**
     * 删除异常城市数据
     * @param id
     */
    void deleteErrorData(int id);

    /**
     * 发起云端数据列表检测需要在初始化观察者IDataInitObserver的回调函数OnInit触发后发起
     */
    void requestDataListCheck();
}
