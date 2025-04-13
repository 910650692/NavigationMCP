package com.fy.navi.service.adapter.mapdata.bls;

import com.autonavi.gbl.data.MapDataService;
import com.autonavi.gbl.data.model.DownLoadMode;
import com.autonavi.gbl.data.observer.IDownloadObserver;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.mapdata.IMapDataApi;
import com.fy.navi.service.adapter.mapdata.MapDataAdapterCallBack;
import com.fy.navi.service.define.bean.AdminCodeBean;
import com.fy.navi.service.define.bean.AreaExtraInfoBean;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;

import java.util.ArrayList;

public class MapDataAdapterImpl implements IMapDataApi {
    private static final String TAG = MapDefaultFinalTag.MAP_DATA_SERVICE_TAG;
    private MapDataService mMapDataService;
    private MapDataObserversHelper mMapDataObserversHelper;

    public MapDataAdapterImpl() {
        mMapDataService = (MapDataService) ServiceMgr.getServiceMgrInstance()
                .getBLService(SingleServiceID.MapDataSingleServiceID);
        mMapDataObserversHelper = new MapDataObserversHelper(mMapDataService);
    }

    @Override
    public void init() {
        mMapDataObserversHelper.initMapDataService();
    }

    @Override
    public void registerCallBack(final String key, final MapDataAdapterCallBack callBack) {
        mMapDataObserversHelper.registerCallBack(key, callBack);
    }

    @Override
    public void unRegisterCallback(final String key, final MapDataAdapterCallBack callBack) {
        mMapDataObserversHelper.unRegisterCallback(key, callBack);
    }

    @Override
    public void unInit() {
        if (mMapDataService != null) {
            mMapDataService.unInit();
            mMapDataService.removeNetDownloadObserver((IDownloadObserver) this);
            mMapDataObserversHelper.removeCallback();
            mMapDataService = null;
        }
    }

    /**
     * 根据经纬度解析获取城市adCode
     * 若解析失败,则返回0
     * 调用该函数只需要获取MapDataService即可，不要求初始化成功
     */
    @Override
    public int getAdCodeByLonLat(final double lon, final double lat) {
        if (null == mMapDataService) {
            return 0;
        }
        return mMapDataService.getAdcodeByLonLat(lon, lat);
    }

    /**
     * 通过adCode获取城市信息
     * 若解析失败,则返回null
     */
    @Override
    public CityDataInfo getCityInfo(final int adCode) {
        return mMapDataObserversHelper.getCityInfo(adCode);
    }

    @Override
    public AreaExtraInfoBean getAreaExtraInfo(final AdminCodeBean adminCode) {
        return mMapDataObserversHelper.getAreaExtraInfo(adminCode);
    }

    /**
     * 获取离线数据版本号
     * @param adCode  当前城市的行政编码
     * @return
     */
    @Override
    public String getDataFileVersion(final int adCode) {
        return mMapDataObserversHelper.getDataFileVersion(adCode);
    }

    /**
     * 设置当前城市的行政编码
     */
    @Override
    public void setCurrentCityAdCode(final int adCode) {
        if (mMapDataService != null) {
            mMapDataService.setCurrentCityAdcode(adCode);
        }
    }

    @Override
    public ArrayList<Integer> getAdCodeList(final int downLoadMode, final String strKey) {
        if (mMapDataService != null) {
            return mMapDataService.getAdcodeList(downLoadMode, strKey);
        }
        return null;
    }

    /**
     * 取得等待中、下载中、暂停、解压中、重试状态下的所有城市adCode列表
     * <p>
     * 离线地图数据下载专用接口
     * 下载模式downLoadMode=DOWNLOAD_MODE_NET时，需要等【首次】RequestDataListCheck请求的观察者监听pObserver回调OnRequestDataListCheck后调用。
     * 下载模式downLoadMode=DOWNLOAD_MODE_USB时，需要每次等RequestDataListCheck请求的观察者监听pObserver回调OnRequestDataListCheck后调用。
     */
    @Override
    public ArrayList<Integer> getWorkingQueueAdCodeList() {
        if (mMapDataService != null) {
            return mMapDataService.getWorkingQueueAdcodeList(DownLoadMode.DOWNLOAD_MODE_NET);
        }
        return null;
    }

    @Override
    public ArrayList<ProvDataInfo> getWorkingList() {
        return mMapDataObserversHelper.getWorkingList();
    }

    @Override
    public ArrayList<ProvDataInfo> getWorkedList() {
        return mMapDataObserversHelper.getWorkedList();
    }

    @Override
    public ArrayList<CityDataInfo> getNearAdCodeList(final int adCode) {
        return mMapDataObserversHelper.getNearAdCodeList(adCode);
    }

    @Override
    public void operate(final int opType, final ArrayList<Integer> adCodeDiyLst) {
         mMapDataObserversHelper.operate(opType, adCodeDiyLst);
    }

    @Override
    public ArrayList<ProvDataInfo> getMapDataList() {
        return mMapDataObserversHelper.getMapDataList();
    }

    @Override
    public CityDataInfo getCountryData() {
        return mMapDataObserversHelper.getCountryData();
    }

    @Override
    public ArrayList<ProvDataInfo> searchAdCode(final String strKey) {
        return mMapDataObserversHelper.searchAdCode(strKey);
    }

    @Override
    public ArrayList<ProvDataInfo> searchDownLoaded(final String strKey) {
        return mMapDataObserversHelper.searchDownLoaded(strKey);
    }

    @Override
    public int searchCityAdCode(final String strKey) {
        return  mMapDataObserversHelper.searchCityAdCode(strKey);
    }

    @Override
    public ArrayList<ProvDataInfo> getAllDownLoadedList() {
        return mMapDataObserversHelper.getAllDownLoadedList();
    }

    @Override
    public void abortDataListCheck(final int downloadMode) {
        if (mMapDataService != null) {
            mMapDataService.abortRequestDataListCheck(downloadMode);
        }
    }

    /**
     * 删除异常城市数据
     * @param id
     */
    @Override
    public void deleteErrorData(final int id) {
        if (mMapDataService != null) {
            mMapDataService.deleteErrorData(id);
        }
    }

    /**
     * 发起云端数据列表检测需要在初始化观察者IDataInitObserver的回调函数OnInit触发后发起
     */
    @Override
    public void requestDataListCheck() {
        mMapDataObserversHelper.requestDataListCheck();
    }
}
