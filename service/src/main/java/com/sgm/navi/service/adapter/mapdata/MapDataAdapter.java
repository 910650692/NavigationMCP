package com.sgm.navi.service.adapter.mapdata;

import com.sgm.navi.service.AdapterConfig;
import com.sgm.navi.service.define.bean.AdminCodeBean;
import com.sgm.navi.service.define.bean.AreaExtraInfoBean;
import com.sgm.navi.service.define.mapdata.CityDataInfo;
import com.sgm.navi.service.define.mapdata.ProvDataInfo;

import java.util.ArrayList;
import java.util.Objects;

public final class MapDataAdapter {
    private static final String CLASS_API_PKG = Objects.requireNonNull(MapDataAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "MapDataAdapterImpl";
    private IMapDataApi mMapDataApi;

    private MapDataAdapter() {
        mMapDataApi = (IMapDataApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
    }

    /**
     * 初始化离线数据
     */
    public void initMapDataService() {
        mMapDataApi.init();
    }

    /**
     * 获取离线数据版本号
     * @param adCode  当前城市的行政编码
     * @return 返回离线数据版本信息
     */
    public String getDataFileVersion(final int adCode) {
        return mMapDataApi.getDataFileVersion(adCode);
    }

    /**
     * 获取所有省份+城市信息
     * @return 返回所有离线数据信息
     */
    public ArrayList<ProvDataInfo> getMapDataList() {
        return mMapDataApi.getMapDataList();
    }

    /**
     * 获取基础功能包数据
     * @return 返回基础包信息
     */
    public CityDataInfo getCountryData() {
        return mMapDataApi.getCountryData();
    }

    /**
     * 获取 已下载的 省份+城市 结构 信息
     * 该方法对外提供
     * @return 返回所有已下载的数据
     */
    public ArrayList<ProvDataInfo> getAllDownLoadedList() {
        return mMapDataApi.getAllDownLoadedList();
    }

    /**
     * 通过搜索关键字获取行政区域adcode列表
     * @param strKey
     * @return 返回匹配关键字的信息
     */
    public ArrayList<ProvDataInfo> searchAdCode(final String strKey) {
        return mMapDataApi.searchAdCode(strKey);
    }

    /**
     * 通过搜索关键字获取已下载的行政区域adcode列表
     * 该方法对外提供使用
     *
     * @param strKey
     * @return 返回匹配到关键的已下载信息
     */
    public ArrayList<ProvDataInfo> searchDownLoaded(final String strKey) {
        return mMapDataApi.searchDownLoaded(strKey);
    }

    /**
     * 通过搜索城市关键字获取行政区域adcode列表
     * @param strKey
     * @return 返回对应的行政区域信息
     */
    public int searchCityAdCode(final String strKey) {
        return mMapDataApi.searchCityAdCode(strKey);
    }

    /**
     * 通过adCode获取城市信息
     * @param adCode
     * @return 返回城市信息
     */
    public CityDataInfo getCityInfo(final int adCode) {
        return mMapDataApi.getCityInfo(adCode);
    }

    /**
     * 获取区域扩展信息
     * @param adminCode
     * @return 返回区域扩展信息
     */
    public AreaExtraInfoBean getAreaExtraInfo(final AdminCodeBean adminCode) {
        return mMapDataApi.getAreaExtraInfo(adminCode);
    }

    /**
     * 通过关键字获取 adcode 列表
     * @param downLoadMode
     * @param strKey
     * @return 返回adCode列表
     */
    public ArrayList<Integer> getAdCodeList(final int downLoadMode, final String strKey) {
        return mMapDataApi.getAdCodeList(downLoadMode, strKey);
    }

    /**
     * 离线地图数据下载专用接口
     * 下载模式downLoadMode=DOWNLOAD_MODE_NET时，需要等【首次】RequestDataListCheck请求的观察者监听pObserver回调OnRequestDataListCheck后调用。
     * 下载模式downLoadMode=DOWNLOAD_MODE_USB时，需要每次等RequestDataListCheck请求的观察者监听pObserver回调OnRequestDataListCheck后调用。
     * @return 返回等待中、下载中、暂停、解压中、重试状态下的所有城市adCode列表
     */
    public ArrayList<Integer> getWorkingQueueAdCodeList() {
        return mMapDataApi.getWorkingQueueAdCodeList();
    }

    /**
     * 获取下载中、更新中状态下的所有城市adCode列表
     * @return 返回下载中的信息
     */
    public ArrayList<ProvDataInfo> getWorkingList() {
        return mMapDataApi.getWorkingList();
    }

    /**
     * 获取已下载状态下的所有城市adCode列表
     * @return 返回已下载的信息
     */
    public ArrayList<ProvDataInfo> getWorkedList() {
        return mMapDataApi.getWorkedList();
    }

    /**
     * 通过adCode获取附近推荐城市信息
     * @param adCode
     * @return 返回附近推荐城市信息
     */
    public ArrayList<CityDataInfo> getNearAdCodeList(final int adCode) {
        return mMapDataApi.getNearAdCodeList(adCode);
    }

    /**
     * 根据经纬度解析获取城市adCode
     * 若解析失败,则返回0
     * 调用该函数只需要获取MapDataService即可，不要求初始化成功
     * @param lon
     * @param lat
     * @return 返回adCode
     */
    public int getAdCodeByLonLat(final double lon, final double lat) {
        return mMapDataApi.getAdCodeByLonLat(lon, lat);
    }

    /**
     * 删除异常城市数据
     * @param id
     */
    public void deleteErrorData(final int id) {
        mMapDataApi.deleteErrorData(id);
    }

    /**
     * 离线数据下载操作
     * @param opType
     * @param adCodeDiyLst
     */
    public void operate(final int opType, final ArrayList<Integer> adCodeDiyLst) {
        mMapDataApi.operate(opType, adCodeDiyLst);
    }

    /**
     * 发起云端数据列表检测
     */
    public void requestDataListCheck() {
        mMapDataApi.requestDataListCheck();
    }

    /**
     * 注册离线信息回调
     * @param key
     * @param callBack
     */
    public void registerCallBack(final String key, final MapDataAdapterCallBack callBack) {
        mMapDataApi.registerCallBack(key, callBack);
    }

    /**
     * 反注册离线信息回调
     * @param key
     * @param callBack
     */
    public void removeCallBack(final String key, final MapDataAdapterCallBack callBack) {
        mMapDataApi.unRegisterCallback(key, callBack);
    }

    /**
     * 反初始化离线数据服务
     */
    public void unInitMapDataService() {
        mMapDataApi.unInit();
    }

    public static MapDataAdapter getInstance() {
        return Helper.RA;
    }

    private static final class Helper {
        private static final MapDataAdapter RA = new MapDataAdapter();
    }
}
