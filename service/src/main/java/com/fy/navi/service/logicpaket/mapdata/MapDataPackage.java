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
import com.fy.navi.service.logicpaket.setting.SettingPackage;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

/**
 * @Description
 * @Author fh
 * @date 2024/12/09
 */
public class MapDataPackage implements MapDataAdapterCallBack {
    private MapDataAdapter mMapDataAdapter;
    private final Hashtable<String, MapDataCallBack> callBacks;
    private MapDataPackage() {
        callBacks = new Hashtable<>();
        mMapDataAdapter = MapDataAdapter.getInstance();
    }

    public void initMapDataService() {
        mMapDataAdapter.initMapDataService();
        mMapDataAdapter.registerCallBack("MapDataPackage", this);
    }

    public synchronized void registerCallBack(String key, MapDataCallBack callback) {
        if (callback != null && !callBacks.contains(callback)) {
            callBacks.put(key, callback);
        }
    }

    /**
     * 获取全部省份+城市数据
     */
    public ArrayList<ProvDataInfo> getMapDataList() {
        return mMapDataAdapter.getMapDataList();
    }

    /**
     * 获取基础功能包数据
     */
    public CityDataInfo getCountryData() {
        return mMapDataAdapter.getCountryData();
    }

    /**
     * 根据行政编码获取城市离线地图各个数据文件版本号
     * @param adCode
     * @return
     */
    public String getDataFileVersion(int adCode) {
        return mMapDataAdapter.getDataFileVersion(adCode);
    }

    /**
     * 获取下载中、更新中状态下的所有城市adCode列表
     * @return
     */
    public ArrayList<CityDataInfo> getWorkingList() {
        return mMapDataAdapter.getWorkingList();
    }

    /**
     * 获取已下载状态下的所有城市adCode列表
     * @return
     */
    public ArrayList<CityDataInfo> getWorkedList() {
        return mMapDataAdapter.getWorkedList();
    }

    /**
     * 通过adCode获取附近推荐城市信息
     * @param adCode
     * @return
     */
    public ArrayList<CityDataInfo> getNearAdCodeList(int adCode) {
        return mMapDataAdapter.getNearAdCodeList(adCode);
    }

    /**
     * 获取 已下载的 省份+城市 结构 信息
     * 该方法对外提供
     * @return
     */
    public ArrayList<ProvDataInfo> getAllDownLoadedList() {
        return mMapDataAdapter.getAllDownLoadedList();
    }

    /**
     * 通过搜索关键字获取行政区域adcode列表
     * @param strKey
     * @return
     */
    public ArrayList<ProvDataInfo> searchAdCode(String strKey) {
        return mMapDataAdapter.searchAdCode(strKey);
    }

    /**
     * 通过搜索关键字获取已下载的行政区域adcode列表
     * 该方法对外提供使用
     *
     * @param strKey
     * @return
     */
    public ArrayList<ProvDataInfo> searchDownLoaded(String strKey) {
        return mMapDataAdapter.searchDownLoaded(strKey);
    }

    /**
     * 通过搜索城市关键字获取行政区域adcode列表
     * @param strKey
     * @return
     */
    public int searchCityAdCode(String strKey) {
        return mMapDataAdapter.searchCityAdCode(strKey);
    }

    /**
     * 通过经纬对获取城市adcode行政编码
     * @param lon
     * @param lat
     * @return
     */
    public int getAdCodeByLonLat(double lon, double lat) {
        return mMapDataAdapter.getAdCodeByLonLat(lon, lat);
    }

    /**
     * 通过adcode获取城市信息
     * @param adCode
     * @return
     */
    public CityDataInfo getCityInfo(int adCode) {
        return mMapDataAdapter.getCityInfo(adCode);
    }

    public AreaExtraInfoBean getAreaExtraInfo(AdminCodeBean adminCode) {
        return mMapDataAdapter.getAreaExtraInfo(adminCode);
    }

    public ArrayList<Integer> getAdCodeList(int downLoadMode, String strKey) {
        return mMapDataAdapter.getAdCodeList(downLoadMode, strKey);
    }

    /**
     *   删除异常城市数据
     * @param id
     */
    public void deleteErrorData(int id) {
        mMapDataAdapter.deleteErrorData(id);
    }

    /**
     * 取消所有下载中的任务
     * 主要是页面销毁前先中止未完成的task,否则ANR
     *
     * @param adCodeList 要取消下载的adCode列表,若为null,则会取消所有未完成的task
     */
    public void cancelAllTask(ArrayList<Integer> adCodeList) {
        if (adCodeList == null || adCodeList.isEmpty()) {
            adCodeList = mMapDataAdapter.getWorkingQueueAdCodeList();
        }
        mMapDataAdapter.operate(OperationType.OPERATION_TYPE_CANCEL.ordinal(), adCodeList);
    }

    /**
     * 删除所有已经下载的任务
     *
     * @param adCodeList 要删除已经下载的adCode列表
     */
    public void deleteAllTask(ArrayList<Integer> adCodeList) {
        mMapDataAdapter.operate(OperationType.OPERATION_TYPE_DELETE.ordinal(), adCodeList);
    }

    /**
     * 暂停所有下载中的任务
     *
     * @param adCodeList 暂停下载操作 ， adcodeList为空时暂停当前进行中的adCodeList
     */
    public void pauseAllTask(ArrayList<Integer> adCodeList) {
        if (adCodeList == null || adCodeList.isEmpty()) {
            adCodeList = mMapDataAdapter.getWorkingQueueAdCodeList();
        }
        mMapDataAdapter.operate(OperationType.OPERATION_TYPE_PAUSE.ordinal(), adCodeList);
    }

    /**
     * 开始所有下载中的任务
     *
     * @param adCodeList 继续下载操作 ， adCodeList为空时继续当前暂停中待继续的adCodeList
     */
    public void startAllTask(ArrayList<Integer> adCodeList) {
        if (adCodeList == null || adCodeList.isEmpty()) {
            adCodeList = mMapDataAdapter.getWorkingQueueAdCodeList();
        }
        mMapDataAdapter.operate(OperationType.OPERATION_TYPE_START.ordinal(), adCodeList);
    }

    public void unInitMapDataService() {
        mMapDataAdapter.unInitMapDataService();
    }

    public static MapDataPackage getInstance() {
        return Helper.ep;
    }

    @Override
    public void onDownLoadStatus(ProvDataInfo provDataInfo) {
        if (null != callBacks) {
            for (MapDataCallBack observer : callBacks.values()) {
                observer.onDownLoadStatus(provDataInfo);
            }
        }
    }

    @Override
    public void onPercent(ProvDataInfo info) {
        if (null != callBacks) {
            for (MapDataCallBack observer : callBacks.values()) {
                observer.onPercent(info);
            }
        }
    }

    @Override
    public void onMergedStatusInfo(MergedStatusBean mergedStatusInfo) {
        if (null != callBacks) {
            for (MapDataCallBack observer : callBacks.values()) {
                observer.onMergedStatusInfo(mergedStatusInfo);
            }
        }
    }

    @Override
    public void onErrorNotify(int downLoadMode, int dataType, int id, int errType, String errMsg) {
        if (null != callBacks) {
            for (MapDataCallBack observer : callBacks.values()) {
                observer.onErrorNotify(downLoadMode, dataType, id, errType, errMsg);
            }
        }
    }

    @Override
    public void onDeleteErrorData(int downLoadMode, int dataType, int id, int opCode) {
        if (null != callBacks) {
            for (MapDataCallBack observer : callBacks.values()) {
                observer.onDeleteErrorData(downLoadMode, dataType, id, opCode);
            }
        }
    }

    private static final class Helper {
        private static final MapDataPackage ep = new MapDataPackage();
    }

}
