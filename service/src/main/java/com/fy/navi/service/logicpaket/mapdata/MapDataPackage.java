package com.fy.navi.service.logicpaket.mapdata;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.adapter.mapdata.MapDataAdapter;
import com.fy.navi.service.adapter.mapdata.MapDataAdapterCallBack;
import com.fy.navi.service.define.bean.AdminCodeBean;
import com.fy.navi.service.define.bean.AreaExtraInfoBean;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.MergedStatusBean;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.service.define.voice.OperationType;

import java.util.ArrayList;
import java.util.Hashtable;

public final class MapDataPackage implements MapDataAdapterCallBack {
    private MapDataAdapter mMapDataAdapter;
    private final Hashtable<String, MapDataCallBack> mCallBacks;
    private MapDataPackage() {
        mCallBacks = new Hashtable<>();
        mMapDataAdapter = MapDataAdapter.getInstance();
    }

    /**
     * 离线数据功能初始化
     */
    public void initMapDataService() {
        mMapDataAdapter.initMapDataService();
        mMapDataAdapter.registerCallBack("MapDataPackage", this);
    }

    /**
     * 注册数据监听
     * @param key
     * @param callback
     */
    public synchronized void registerCallBack(final String key, final MapDataCallBack callback) {
        if (callback != null && !mCallBacks.contains(callback)) {
            mCallBacks.put(key, callback);
        }
    }

    /**
     * 获取全部省份+城市数据
     * @return 返回离全部线数据列表
     */
    public ArrayList<ProvDataInfo> getMapDataList() {
        return mMapDataAdapter.getMapDataList();
    }

    /**
     * 获取基础功能包数据
     * @return 返回基础包信息
     */
    public CityDataInfo getCountryData() {
        return mMapDataAdapter.getCountryData();
    }

    /**
     * 根据行政编码获取城市离线地图各个数据文件版本号
     * @param adCode
     * @return 返回数据版本信息
     */
    public String getDataFileVersion(final int adCode) {
        return mMapDataAdapter.getDataFileVersion(adCode);
    }

    /**
     * 获取下载中、更新中状态下的所有城市adCode列表
     * @return 返回下载中/更新中的数据信息
     */
    public ArrayList<CityDataInfo> getWorkingList() {
        return mMapDataAdapter.getWorkingList();
    }

    /**
     * 获取所有下载中的数据列表
     * @param cityDataInfos
     * @return 返回所有下载中的数据列表
     */
    public ArrayList<Integer> getAllWorkingAdCodeList(final ArrayList<CityDataInfo> cityDataInfos) {
        final ArrayList<Integer> adCodeList = new ArrayList<>();
        for (CityDataInfo cityDataInfo : cityDataInfos) {
            adCodeList.add(cityDataInfo.getAdcode());
        }
        return adCodeList;
    }

    /**
     * 获取已下载状态下的所有城市adCode列表
     * @return 返回已下载的数据信息
     */
    public ArrayList<CityDataInfo> getWorkedList() {
        return mMapDataAdapter.getWorkedList();
    }

    /**
     * 通过adCode获取附近推荐城市信息
     * @param adCode
     * @return 返回附近推荐城市信息
     */
    public ArrayList<CityDataInfo> getNearAdCodeList(final int adCode) {
        return mMapDataAdapter.getNearAdCodeList(adCode);
    }

    /**
     * 获取 已下载的 省份+城市 结构 信息
     * 该方法对外提供
     * @return 返回已下载数据信息
     */
    public ArrayList<ProvDataInfo> getAllDownLoadedList() {
        return mMapDataAdapter.getAllDownLoadedList();
    }

    /**
     * 通过搜索关键字获取行政区域adcode列表
     * @param strKey
     * @return 返回根据关键字获取到数据列表
     */
    public ArrayList<ProvDataInfo> searchAdCode(final String strKey) {
        return mMapDataAdapter.searchAdCode(strKey);
    }

    /**
     * 通过搜索关键字获取已下载的行政区域adcode列表
     * 该方法对外提供使用
     *
     * @param strKey
     * @return 返回根据关键字获取到已下载数据列表
     */
    public ArrayList<ProvDataInfo> searchDownLoaded(final String strKey) {
        return mMapDataAdapter.searchDownLoaded(strKey);
    }

    /**
     * 通过搜索城市关键字获取行政区域adcode列表
     * @param strKey
     * @return 返回城市code
     */
    public int searchCityAdCode(final String strKey) {
        return mMapDataAdapter.searchCityAdCode(strKey);
    }

    /**
     * 通过经纬对获取城市adcode行政编码
     * @param lon
     * @param lat
     * @return 返回行政编码
     */
    public int getAdCodeByLonLat(final double lon, final double lat) {
        return mMapDataAdapter.getAdCodeByLonLat(lon, lat);
    }

    /**
     * 通过adcode获取城市信息
     * @param adCode
     * @return 返回城市信息
     */
    public CityDataInfo getCityInfo(final int adCode) {
        return mMapDataAdapter.getCityInfo(adCode);
    }

    /**
     * 根据code获取区域信息
     * @param adminCode
     * @return 返回区域信息
     */
    public AreaExtraInfoBean getAreaExtraInfo(final AdminCodeBean adminCode) {
        return mMapDataAdapter.getAreaExtraInfo(adminCode);
    }

    /**
     *   删除异常城市数据
     * @param id
     */
    public void deleteErrorData(final int id) {
        mMapDataAdapter.deleteErrorData(id);
    }

    /**
     * 取消所有下载中的任务
     * 主要是页面销毁前先中止未完成的task,否则ANR
     *
     * @param adCodeList 要取消下载的adCode列表,若为null,则会取消所有未完成的task
     */
    public void cancelAllTask(final ArrayList<Integer> adCodeList) {
        mMapDataAdapter.operate(OperationType.OPERATION_TYPE_CANCEL.ordinal(), adCodeList);
    }

    /**
     * 删除所有已经下载的任务
     *
     * @param adCodeList 要删除已经下载的adCode列表
     */
    public void deleteAllTask(final ArrayList<Integer> adCodeList) {
        mMapDataAdapter.operate(OperationType.OPERATION_TYPE_DELETE.ordinal(), adCodeList);
    }

    /**
     * 暂停所有下载中的任务
     *
     * @param adCodeList 暂停下载操作 ， adcodeList为空时暂停当前进行中的adCodeList
     */
    public void pauseAllTask(final ArrayList<Integer> adCodeList) {
        mMapDataAdapter.operate(OperationType.OPERATION_TYPE_PAUSE.ordinal(), adCodeList);
    }

    /**
     * 开始所有下载中的任务
     *
     * @param adCodeList 继续下载操作 ， adCodeList为空时继续当前暂停中待继续的adCodeList
     */
    public void startAllTask(final ArrayList<Integer> adCodeList) {
        mMapDataAdapter.operate(OperationType.OPERATION_TYPE_START.ordinal(), adCodeList);
    }

    /**
     * 发起云端数据列表检测
     */
    public void requestDataListCheck() {
        mMapDataAdapter.requestDataListCheck();
    }

    public static MapDataPackage getInstance() {
        return Helper.EP;
    }

    @Override
    public void onDownLoadStatus(final ProvDataInfo provDataInfo) {
        Logger.d("MapDataPackage","onDownLoadStatus -> provDataInfo: " + GsonUtils.toJson(provDataInfo));

        if (null != mCallBacks) {
            for (MapDataCallBack observer : mCallBacks.values()) {
                observer.onDownLoadStatus(provDataInfo);
            }
        }
    }

    @Override
    public void onMergedStatusInfo(final MergedStatusBean mergedStatusInfo) {
        if (null != mCallBacks) {
            for (MapDataCallBack observer : mCallBacks.values()) {
                observer.onMergedStatusInfo(mergedStatusInfo);
            }
        }
    }

    @Override
    public void onErrorNotify(final int downLoadMode, final int dataType, final int id,
                              final int errType, final String errMsg) {
        if (null != mCallBacks) {
            for (MapDataCallBack observer : mCallBacks.values()) {
                observer.onErrorNotify(downLoadMode, dataType, id, errType, errMsg);
            }
        }
    }

    @Override
    public void onDeleteErrorData(final int downLoadMode, final int dataType, final int id, final int opCode) {
        if (null != mCallBacks) {
            for (MapDataCallBack observer : mCallBacks.values()) {
                observer.onDeleteErrorData(downLoadMode, dataType, id, opCode);
            }
        }
    }

    @Override
    public void onRequestCheckSuccess(final int downLoadMode, final int dataType, final int opCode) {
        if (null != mCallBacks) {
            for (MapDataCallBack observer : mCallBacks.values()) {
                observer.onRequestCheckSuccess(downLoadMode, dataType, opCode);
            }
        }
    }

    private static final class Helper {
        private static final MapDataPackage EP = new MapDataPackage();
    }

}
