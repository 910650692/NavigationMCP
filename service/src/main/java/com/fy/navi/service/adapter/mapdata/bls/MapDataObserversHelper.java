package com.fy.navi.service.adapter.mapdata.bls;

import android.annotation.SuppressLint;

import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.data.MapDataService;
import com.autonavi.gbl.data.model.AdminCode;
import com.autonavi.gbl.data.model.Area;
import com.autonavi.gbl.data.model.AreaExtraInfo;
import com.autonavi.gbl.data.model.AreaType;
import com.autonavi.gbl.data.model.CityDownLoadItem;
import com.autonavi.gbl.data.model.CityItemInfo;
import com.autonavi.gbl.data.model.DataInitParam;
import com.autonavi.gbl.data.model.DownLoadMode;
import com.autonavi.gbl.data.model.InitConfig;
import com.autonavi.gbl.data.model.MapDataFileType;
import com.autonavi.gbl.data.model.MapDataMode;
import com.autonavi.gbl.data.model.MergedStatusInfo;
import com.autonavi.gbl.data.model.ProvinceInfo;
import com.autonavi.gbl.data.model.TaskStatusCode;
import com.autonavi.gbl.data.observer.IDataInitObserver;
import com.autonavi.gbl.data.observer.IDataListObserver;
import com.autonavi.gbl.data.observer.IDownloadObserver;
import com.autonavi.gbl.data.observer.IErrorDataObserver;
import com.autonavi.gbl.data.observer.IMergedStatusInfoObserver;
import com.autonavi.gbl.util.errorcode.common.Service;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.adapter.mapdata.MapDataAdapterCallBack;
import com.fy.navi.service.define.bean.AdMapPointBean;
import com.fy.navi.service.define.bean.AdminCodeBean;
import com.fy.navi.service.define.bean.AreaExtraInfoBean;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.CityDownLoadInfo;
import com.fy.navi.service.define.mapdata.MergedStatusBean;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.service.greendao.CommonManager;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

public class MapDataObserversHelper implements IDataInitObserver, IDownloadObserver, IMergedStatusInfoObserver,
        IErrorDataObserver, IDataListObserver {
    private static final String TAG = MapDataObserversHelper.class.getSimpleName();
    private static final String TAG_DATA_TYPE = "; dataType = ";
    private static final String TAG_ID = "; id = ";
    private MapDataService mMapDataService;
    private int mInitCode;
    /*** 该集合只存放离线数据结果回调 key = 哪个类请求的，value = requestResult 理论上集合的长度永远为1**/
    private Hashtable<String, MapDataAdapterCallBack> mapDataResultObserverHashtable;
    private CommonManager mCommonManager;
    private String mDataVersion = "0_000000";
    private String mSpaceStr = "_";
    private String mZeroStr = "0";
    private String mOneStr = "1";

    protected MapDataObserversHelper(final MapDataService mapDataService) {
        mapDataResultObserverHashtable = new Hashtable<>();
        mMapDataService = mapDataService;
        mCommonManager = CommonManager.getInstance();
        mCommonManager.init();
    }

    protected void initMapDataService() {
        final InitConfig config = new InitConfig();
        //下载路径，设置后没用
        config.strStoredPath = "";
        //all_city_compile.json、global.db文件目录
        config.strConfigfilePath = GBLCacheFilePath.OFFLINE_CONF_PATH;
        //数据下载存放路径，目前仅地图下载支持该参数配置，语音下载暂不支持
        config.strDownloadPath = GBLCacheFilePath.OFFLINE_DOWNLOAD_DIR;
        final ArrayList<DataInitParam> extendedList = new ArrayList<>(1);
        config.extendedParamList = extendedList;
        // 设置磁盘空间安全阈值（默认设置为80MB）
        config.nThresholdValue = 80;
        // 设置地图数据模式,默认为：基础地图数据模式
        config.mapDataMode = MapDataMode.MAP_DATA_MODE_BASE;
        // 初始化离线地图数据服务
        mInitCode = mMapDataService.init(config, this);
        Logger.d(TAG, "initService: mInitCode = " + mInitCode);

        mMapDataService.addNetDownloadObserver(this);
        //设置异常数据监听观察者
        mMapDataService.setErrorDataObserver(this);
        mMapDataService.setIMergedStatusInfoObserver(this);
        final ArrayList<Integer> list = mMapDataService.getDownLoadAdcodeList();
        final String value = ConvertUtils.isEmpty(list) ? "0" : "1";
        mCommonManager.insertOrReplace(UserDataCode.SETTING_DOWNLOAD_LIST, value);
    }

    /**
     * 注册离线信息回调
     * @param key
     * @param callBack
     */
    public void registerCallBack(final String key, final MapDataAdapterCallBack callBack) {
        mapDataResultObserverHashtable.put(key, callBack);
    }

    /**
     * 反注册离线信息回调
     * @param key
     * @param callBack
     */
    public void unRegisterCallback(final String key, final MapDataAdapterCallBack callBack) {
        mapDataResultObserverHashtable.remove(key, callBack);
    }

    /**
     * 移除注册离线信息回调
     */
    public void removeCallback() {
        mapDataResultObserverHashtable.clear();
    }

    /**
     * sdk 数据转换到本地define类
     * @param cityArea
     * @return 返回本地define定义类
     */
    private CityDataInfo convertCityData(final Area cityArea) {
        final CityDataInfo cityDataInfo = new CityDataInfo();
        if (!ConvertUtils.isEmpty(cityArea)) {
            cityDataInfo.setAdcode(cityArea.adcode);
            cityDataInfo.setAreaType(cityArea.areaType);
            cityDataInfo.setName(cityArea.name);
            cityDataInfo.setJianPin(cityArea.jianPin);
            cityDataInfo.setPinYin(cityArea.pinYin);
            cityDataInfo.setUpperAdcode(cityArea.upperAdcode);
            cityDataInfo.setVecNearAdcodeList(cityArea.vecNearAdcodeList);
            cityDataInfo.setVecLowerAdcodeList(cityArea.vecLowerAdcodeList);
            // 获取城市数据包的下载状态信息
            cityDataInfo.setDownLoadInfo(getCityDownLoadInfo(cityArea.adcode));
        }
        return cityDataInfo;
    }

    /**
     * 全省地图数据按照city info 处理
     * @param cityBeanList
     * @param provAdcode
     * @return 返回本地define定义类
     */
    private CityDataInfo convertAllCityData(final ArrayList<CityDataInfo> cityBeanList, final int provAdcode) {
        final CityDataInfo cityDataInfo = new CityDataInfo();
        cityDataInfo.setAdcode(provAdcode);
        cityDataInfo.setAreaType(3);
        cityDataInfo.setName("全省下载");
        cityDataInfo.setJianPin("qsdt");
        cityDataInfo.setPinYin("quanshegnditu");
        cityDataInfo.setUpperAdcode(provAdcode);
        cityDataInfo.setVecNearAdcodeList(null);
        cityDataInfo.setVecLowerAdcodeList(null);

        final Map<Integer, Integer> map = new HashMap<>();
        BigInteger sum = BigInteger.ZERO;
        if (cityBeanList != null && !cityBeanList.isEmpty()) {
            for (CityDataInfo info : cityBeanList) {
                //获取附近城市数据包大小总和
                sum =  sum.add(info.getDownLoadInfo().getFullZipSize());
                map.put(info.getAdcode(), info.getDownLoadInfo().getTaskState());
            }
        }
        final CityDownLoadInfo cityDownLoadBean = new CityDownLoadInfo();
        cityDownLoadBean.setAdcode(provAdcode);
        cityDownLoadBean.setAllCityTaskStateMap(map);
        cityDownLoadBean.setFullZipSize(sum);
        cityDataInfo.setDownLoadInfo(cityDownLoadBean);

        return cityDataInfo;
    }

    /**
     * 获取基础功能包数据
     * @return 返回基础包信息
     */
    public CityDataInfo getCountryData() {
        if (mMapDataService != null) {
            // 基础功能包adcode（值固定为0） 目前国家类型行政编码，仅为一个元素，值固定为0
            final Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, 0);
            if (cityArea == null) {
                Logger.e(TAG, "cityArea is null");
                return null;
            }
            return getCityInfo(cityArea.adcode);
        } else {
            return null;
        }
    }

    /**
     * 获取直辖市数据
     * @return 返回直辖市信息
     */
    public ProvDataInfo getDirectDataInfo() {
        if (mMapDataService != null) {
            final ProvDataInfo provDataInfo = new ProvDataInfo();
            // 获取直辖市adcode列表  [110000,500000,310000,120000],按拼音顺序为北京市、重庆市、上海市、天津市
            final ArrayList<Integer> adCodeDirectList = mMapDataService.getAdcodeList(DownLoadMode.DOWNLOAD_MODE_NET, AreaType.AREA_TYPE_DIRECT);
            if (null != adCodeDirectList && !adCodeDirectList.isEmpty()) {
                // 获取对应直辖市下的城市列表
                final ArrayList<CityDataInfo> directCityList = new ArrayList<>();
                for (int i = 0; i < adCodeDirectList.size(); i++) {
                    final Integer cityAdcode = adCodeDirectList.get(i);
                    final Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, cityAdcode);
                    if (cityArea != null) {
                        directCityList.add(getCityInfo(cityArea.adcode));
                    }
                }
                provDataInfo.setName("直辖市");
                provDataInfo.setAreaType(2);
                provDataInfo.setAdcode(2);
                provDataInfo.setJianPin("zxs");
                provDataInfo.setPinYin("zhixiashi");
                provDataInfo.setCityInfoList(directCityList);
            }
            return provDataInfo;
        } else {
            return null;
        }
    }

    /**
     * 获取特别行政区数据
     * @return 返回行政区信息
     */
    public ProvDataInfo getSpecialDataInfo() {
        if (mMapDataService != null) {
            final ProvDataInfo provDataInfo = new ProvDataInfo();
            // 获取特别行政区adCode列表 [820000,810000,710000],按拼音顺序为澳门、香港、台湾（暂无台湾数据）
            final ArrayList<Integer> adCodeSpecialLst = mMapDataService.getAdcodeList(DownLoadMode.DOWNLOAD_MODE_NET, AreaType.AREA_TYPE_SPECIAL);
            if (null != adCodeSpecialLst && !adCodeSpecialLst.isEmpty()) {
                // 获取对应特别行政区下的城市列表
                final ArrayList<CityDataInfo> directCityList = new ArrayList<>();
                for (int i = 0; i < adCodeSpecialLst.size(); i++) {
                    final Integer cityAdcode = adCodeSpecialLst.get(i);
                    final Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, cityAdcode);
                    if (cityArea != null) {
                        directCityList.add(getCityInfo(cityArea.adcode));
                    }
                }
                provDataInfo.setName("特别行政区");
                provDataInfo.setAreaType(4);
                provDataInfo.setAdcode(4);
                provDataInfo.setJianPin("tbxzq");
                provDataInfo.setPinYin("tebiexingzhegnqu");
                provDataInfo.setCityInfoList(directCityList);
            }
            return provDataInfo;
        } else {
            return null;
        }
    }

    /**
     * 获取所有省份+城市信息
     * @return 返回所有离线数据信息
     */
    public ArrayList<ProvDataInfo> getMapDataList() {
        if (mMapDataService != null) {
            final ArrayList<ProvDataInfo> provinceBeanList = new ArrayList<>();

            // 1，添加一级直辖市对应信息
            provinceBeanList.add(getDirectDataInfo());

            // 2，获取普通省份列表（按拼音排序）
            final ArrayList<Integer> adCodeProvLst = mMapDataService.getAdcodeList(DownLoadMode.DOWNLOAD_MODE_NET, AreaType.AREA_TYPE_PROV);
            if (!adCodeProvLst.isEmpty()) {
                for (int i = 0; i < adCodeProvLst.size(); i++) {
                    final Integer provAdcode = adCodeProvLst.get(i);
                    ProvDataInfo provDataInfo = new ProvDataInfo();
                    final ProvinceInfo info = mMapDataService.getProvinceInfo(provAdcode);
                    provDataInfo.setName(info.provName);
                    provDataInfo.setAreaType(info.provLevel);
                    provDataInfo.setAdcode(info.provAdcode);
                    provDataInfo.setJianPin(info.provInitial);
                    provDataInfo.setPinYin(info.provPinyin);
                    final ArrayList<CityDataInfo> cityDataInfos = new ArrayList<>();
                    for (int j = 0; j < info.cityInfoList.size(); j++) {
                        final int cityAdcode = info.cityInfoList.get(j).cityAdcode;
                        final Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, cityAdcode);
                        cityDataInfos.add(convertCityData(cityArea));
                    }
                    provDataInfo.setCityInfoList(cityDataInfos);

                    cityDataInfos.add(0, convertAllCityData(cityDataInfos, provDataInfo.getAdcode())); // 增加“全省地图”item
                    provinceBeanList.add(provDataInfo);
                }
            }
            // 3，添加一级特别行政区对应信息
            provinceBeanList.add(getSpecialDataInfo());
            return provinceBeanList;
        } else {
            return null;
        }
    }

    /**
     * sdk 数据转换到本地define类
     * @param provArea
     * @return 返回本地define定义类
     */
    private ProvDataInfo convertProvData(final Area provArea) {
        final ProvDataInfo provDataInfo = new ProvDataInfo();
        if (!ConvertUtils.isEmpty(provArea)) {
            provDataInfo.setAdcode(provArea.adcode);
            provDataInfo.setAreaType(provArea.areaType);
            provDataInfo.setName(provArea.name);
            provDataInfo.setJianPin(provArea.jianPin);
            provDataInfo.setPinYin(provArea.pinYin);
            provDataInfo.setUpperAdcode(provArea.upperAdcode);
            provDataInfo.setVecNearAdcodeList(provArea.vecNearAdcodeList);
            provDataInfo.setVecLowerAdcodeList(provArea.vecLowerAdcodeList);
            // 获取省份数据包的下载状态信息
//        provDataInfo.setDownLoadInfo(getCityDownLoadInfo(provArea.adcode));
        }
        return provDataInfo;
    }

    /**
     * 获取城市数据包的下载状态信息
     * getCityDownLoadItem 离线地图数据下载专用接口
     * 下载模式downLoadMode=DOWNLOAD_MODE_NET时，需要等【首次】RequestDataListCheck请求的观察者监听pObserver回调OnRequestDataListCheck后调用。
     * 下载模式downLoadMode=DOWNLOAD_MODE_USB时，需要每次等RequestDataListCheck请求的观察者监听pObserver回调OnRequestDataListCheck后调用。
     *
     * @param adCode
     * @return 返回城市包下载信息
     */
    public CityDownLoadInfo getCityDownLoadInfo(final int adCode) {
        if (mMapDataService != null) {
            //通过adcode获取城市下载项信息
            final CityDownLoadItem downloadItem = mMapDataService.getCityDownLoadItem(DownLoadMode.DOWNLOAD_MODE_NET, adCode);
            final CityDownLoadInfo cityDownLoadBean = new CityDownLoadInfo();
            if (downloadItem != null) {
                cityDownLoadBean.setAdcode(downloadItem.adcode);
                cityDownLoadBean.setValidItem(downloadItem.bValidItem);
                cityDownLoadBean.setTaskState(downloadItem.taskState);
                cityDownLoadBean.setErrCode(downloadItem.errCode);
                cityDownLoadBean.setPercent(downloadItem.percent);
                cityDownLoadBean.setUpdate(downloadItem.bUpdate);
                cityDownLoadBean.setIsDataUsed(downloadItem.bIsDataUsed);
                cityDownLoadBean.setCompletelyHighVer(downloadItem.IsCompltelyHighVer);
                cityDownLoadBean.setFullUnpackSize(downloadItem.nFullUnpackSize);
                cityDownLoadBean.setFullZipSize(downloadItem.nFullZipSize);
                cityDownLoadBean.setUnpackSize(downloadItem.nUnpackSize);
                cityDownLoadBean.setZipSize(downloadItem.nZipSize);

                // 客户端根据 downloadItem.bIsDataUsed 和 downloadItem.taskState 字段转换为前端需要显示的文案
                cityDownLoadBean.setStatusTip(switchTaskStatusCodeToString(downloadItem.bUpdate,
                        downloadItem.taskState, downloadItem.IsCompltelyHighVer));
            }
            return cityDownLoadBean;
        }
        return null;
    }

    /**
     * 通过adCode获取城市信息
     * @param adCode
     * @return 返回城市信息
     */
    public CityDataInfo getCityInfo(final int adCode) {
        if (mMapDataService != null) {
            final CityItemInfo info = mMapDataService.getCityInfo(adCode);
            final CityDataInfo cityInfo = new CityDataInfo();
            if (info != null) {
                cityInfo.setUpperAdcode(info.belongedProvince);
                if (info.cityAdcode == 0) {
                    cityInfo.setName("基础功能包");
                } else {
                    cityInfo.setName(info.cityName);
                }
                cityInfo.setAreaType(info.cityLevel);
                cityInfo.setAdcode(info.cityAdcode);
                cityInfo.setJianPin(info.initial);
                cityInfo.setPinYin(info.pinyin);
                cityInfo.setCityX(info.cityX);
                cityInfo.setCityY(info.cityY);
                cityInfo.setDownLoadInfo(getCityDownLoadInfo(adCode));
            }
            Logger.d(TAG, "getCityInfo: cityInfo = " + GsonUtils.toJson(cityInfo));
            return cityInfo;
        }
        Logger.d(TAG, "getCityInfo: null");
        return null;
    }

    /**
     * 根据 downloadItem.bIsDataUsed 和 downloadItem.taskState 字段转换为前端需要显示的文案
     * @param bUpdate
     * @param taskCode
     * @param isCompltelyHighVer
     *
     * @return 返回下载提示文字
     */
    private String switchTaskStatusCodeToString(final boolean bUpdate, final int taskCode, final boolean isCompltelyHighVer) {
        String desc = new String("");
        switch (taskCode) {
            case TaskStatusCode.TASK_STATUS_CODE_READY:
                if (bUpdate) {
                    desc = "更新";
                } else {
                    desc = "下载";
                }
                break;
            case TaskStatusCode.TASK_STATUS_CODE_WAITING:
                desc = "等待中";
                break;
            case TaskStatusCode.TASK_STATUS_CODE_PAUSE:
                desc = "继续";
                break;
            case TaskStatusCode.TASK_STATUS_CODE_DOING:
            case TaskStatusCode.TASK_STATUS_CODE_DONE:
                desc = "下载中";
                if(bUpdate) {
                    desc = "更新中";
                }
                break;
            case TaskStatusCode.TASK_STATUS_CODE_CHECKING:
                desc = "校验中";
                break;
            case TaskStatusCode.TASK_STATUS_CODE_CHECKED:
                desc = "校验完成";
                break;
            case TaskStatusCode.TASK_STATUS_CODE_UNZIPPING:
                desc = "解压中";
                break;
            case TaskStatusCode.TASK_STATUS_CODE_UNZIPPED:
                desc = "解压完成";
                break;
            case TaskStatusCode.TASK_STATUS_CODE_SUCCESS:
                desc = "已下载";
                break;
            case TaskStatusCode.TASK_STATUS_CODE_ERR:
            case TaskStatusCode.TASK_STATUS_CODE_MAX:
                desc = "重试";
                break;
            default:
                break;
        }
        return desc;
    }

    /**
     * 获取下载中、更新中状态下的所有城市adCode列表
     * @return 返回下载中的信息
     */
    public ArrayList<ProvDataInfo> getWorkingList() {
        if (mMapDataService == null) {
            return null;
        }
        // 1，获取下载中、更新中状态下的所有城市adCode列表
        final ArrayList<Integer> downLoadAdcodeList = mMapDataService.getWorkingQueueAdcodeList(DownLoadMode.DOWNLOAD_MODE_NET);
        Logger.d(TAG, "getWorkingList: downLoadAdcodeList" + GsonUtils.toJson(downLoadAdcodeList));
        // 2，根据adCode获取整理后的省份+城市列表
        final ArrayList<ProvDataInfo> provinceBeanList = getWorkDataList(downLoadAdcodeList);
        Logger.d(TAG, "getWorkingList: provinceBeanList = " + GsonUtils.toJson(provinceBeanList));
        return provinceBeanList;
    }

    /**
     * 获取已下载状态下的所有城市adCode列表
     * @return 返回已下载的CityDataInfo信息
     */
    public ArrayList<ProvDataInfo> getWorkedList() {
        if (mMapDataService == null) {
            return null;
        }
        // 1，获取本地已存在数据的adcode列表信息
        final ArrayList<Integer> downLoadAdcodeList = mMapDataService.getDownLoadAdcodeList();
        Logger.d(TAG, "getWorkedList: downLoadAdcodeList" + GsonUtils.toJson(downLoadAdcodeList));
        // 2，根据adCode获取整理后的省份+城市列表
        final ArrayList<ProvDataInfo> provinceBeanList = getWorkDataList(downLoadAdcodeList);
        Logger.d(TAG, "getWorkedList: downloadedList = " + GsonUtils.toJson(provinceBeanList));
        return provinceBeanList;
    }

    /**
     * 下载管理数据转换处理
     * @param downLoadAdcodeList
     * @return
     */
    public ArrayList<ProvDataInfo> getWorkDataList( final ArrayList<Integer> downLoadAdcodeList) {
        // 1，获取省份列表
        final ArrayList<ProvDataInfo> provDataInfoList = new ArrayList<>();
        // 2, 获取省份下的城市列表
        ArrayList<CityDataInfo> cityInfoList = new ArrayList<>();
        ArrayList<CityDataInfo> cityInfoList1 = new ArrayList<>();
        ArrayList<CityDataInfo> cityInfoList2 = new ArrayList<>();
        Map<Integer, ArrayList<CityDataInfo>> provinceMap = new HashMap<>();

        Set<String> seen = new LinkedHashSet<>(); // 使用LinkedHashSet保持顺序
        if (null != downLoadAdcodeList && !downLoadAdcodeList.isEmpty()) {
            for (int i = 0; i < downLoadAdcodeList.size(); i++) {
                final  Integer cityAdCode = downLoadAdcodeList.get(i);
                final Area area = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, cityAdCode);
                if (area == null) {
                    Logger.e(TAG, "area is null");
                    continue;
                }
                ProvDataInfo provDataInfo = new ProvDataInfo();
                if (area.areaType == 0) { // 基础包
                    provDataInfo.setName("基础功能包");
                    provDataInfo.setAreaType(0);
                    provDataInfo.setAdcode(0);
                    provDataInfo.setJianPin("jcgnb");
                    provDataInfo.setPinYin("jichugongnengbao");
                    provDataInfo.setExpanded(true);
                    CityDataInfo cityDataInfo = getCityInfo(area.adcode);
                    cityInfoList.add(cityDataInfo);
                    provDataInfo.setDownLoadInfo(cityDataInfo.getDownLoadInfo()); // 基础包在下载管理页中当做一级省份处理
                    provDataInfo.setCityInfoList(cityInfoList);
                } else if (area.areaType == 2) { // 直辖市
                    provDataInfo.setName("直辖市");
                    provDataInfo.setAreaType(2);
                    provDataInfo.setAdcode(2);
                    provDataInfo.setJianPin("zxs");
                    provDataInfo.setPinYin("zhixiashi");
                    provDataInfo.setExpanded(true);
                    CityDataInfo cityDataInfo = getCityInfo(area.adcode);
                    cityInfoList1.add(cityDataInfo);
                    provDataInfo.setCityInfoList(cityInfoList1);
                } else if (area.areaType == 4) { // 特别行政区
                    provDataInfo.setName("特别行政区");
                    provDataInfo.setAreaType(4);
                    provDataInfo.setAdcode(4);
                    provDataInfo.setJianPin("tbxzq");
                    provDataInfo.setPinYin("tebiexingzhegnqu");
                    provDataInfo.setExpanded(true);
                    CityDataInfo cityDataInfo = getCityInfo(area.adcode);
                    cityInfoList2.add(cityDataInfo);
                    provDataInfo.setCityInfoList(cityInfoList2);
                } else { // 普通省份
                    final ProvinceInfo info = mMapDataService.getProvinceInfo(area.upperAdcode);
                    provDataInfo.setName(info.provName);
                    provDataInfo.setAreaType(info.provLevel);
                    provDataInfo.setAdcode(info.provAdcode);
                    provDataInfo.setJianPin(info.provInitial);
                    provDataInfo.setPinYin(info.provPinyin);
                    provDataInfo.setExpanded(true);
                    CityDataInfo cityDataInfo = getCityInfo(area.adcode);
                    ArrayList<CityDataInfo> cityDataInfoList = provinceMap.get(provDataInfo.getAdcode());
                    if (cityDataInfoList == null) {
                        cityDataInfoList = new ArrayList<>();
                    }
                    cityDataInfoList.add(cityDataInfo);
                    provinceMap.put(provDataInfo.getAdcode(), cityDataInfoList);
                    provDataInfo.setCityInfoList(cityDataInfoList);
                }

                if (seen.add(provDataInfo.getName())) { // 如果set中没有该元素，add返回true
                    provDataInfoList.add(provDataInfo);
                }

            }
        }

        return provDataInfoList;
    }

    /**
     * 获取 已下载的 省份+城市 结构 信息
     * 该方法对外提供
     *
     * @return 返回所有已下载的数据
     */
    public ArrayList<ProvDataInfo> getAllDownLoadedList() {
        if (mMapDataService == null) {
            return null;
        }

        final ArrayList<ProvDataInfo> provinceBeanList = new ArrayList<>();

        // 获取本地已存在数据的adcode列表信息
        final ArrayList<Integer> downLoadAdcodeList = mMapDataService.getDownLoadAdcodeList();
        Logger.d(TAG, "getDownLoadAdCodeList: downLoadAdcodeList" + GsonUtils.toJson(downLoadAdcodeList));

        if (null != downLoadAdcodeList && !downLoadAdcodeList.isEmpty()) {

            //获取已下载省份code列表
            final ArrayList<Integer> provAdcodeList = new ArrayList<>();
            for (int i = 0; i < downLoadAdcodeList.size(); i++) {
                final Integer downLoadAdCode = downLoadAdcodeList.get(i);
                final Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, downLoadAdCode);
                if (cityArea != null && !provAdcodeList.contains(cityArea.upperAdcode)) {
                    provAdcodeList.add(cityArea.upperAdcode);
                }
            }

            //获取省份中已下载的城市信息
            for (int i = 0; i < provAdcodeList.size(); i++) {
                final Integer provAdcode = provAdcodeList.get(i);
                //对省份信息进行赋值
                final ProvDataInfo provDataInfo = new ProvDataInfo();
                if (provAdcode == 0) {
                    provDataInfo.setName("直辖市");
                    provDataInfo.setAreaType(2);
                    provDataInfo.setAdcode(2);
                    provDataInfo.setJianPin("zxs");
                    provDataInfo.setPinYin("zhixiashi");
                } else {
                    final ProvinceInfo info = mMapDataService.getProvinceInfo(provAdcode);
                    provDataInfo.setName(info.provName);
                    provDataInfo.setAreaType(info.provLevel);
                    provDataInfo.setAdcode(info.provAdcode);
                    provDataInfo.setJianPin(info.provInitial);
                    provDataInfo.setPinYin(info.provPinyin);
                }

                //获取省份下已下载的城市
                final ArrayList<CityDataInfo> cityBeanList = new ArrayList<>();
                for (int j = 0; j < downLoadAdcodeList.size(); j++) {
                    final Integer downLoadAdCode = downLoadAdcodeList.get(j);
                    if (downLoadAdCode != 0) { // 移除基础功能包
                        final Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, downLoadAdCode);
                        if (cityArea != null) {
                            final CityDataInfo cityDataInfo = getCityInfo(cityArea.adcode);
                            if (cityDataInfo.getUpperAdcode() == provDataInfo.getAdcode()) {
                                cityBeanList.add(cityDataInfo);
                            }
                        }
                    }
                }

                provDataInfo.setCityInfoList(cityBeanList);

                provinceBeanList.add(provDataInfo);

            }

        }

        Logger.d(TAG, "getDownLoadAdCodeList: provinceBeanList = " + GsonUtils.toJson(provinceBeanList));

        return provinceBeanList;
    }

    /**
     * 通过搜索关键字获取行政区域adcode列表
     * @param strKey
     * @return 返回匹配关键字的信息
     */
    public ArrayList<ProvDataInfo> searchAdCode(final String strKey) {
        if (mMapDataService != null) {
            final ArrayList<ProvDataInfo> provinceBeanList = new ArrayList<>();
            // 获取adCode列表
            final ArrayList<Integer> adCodeProvLst = mMapDataService.searchAdcode(strKey);
            if (null != adCodeProvLst && !adCodeProvLst.isEmpty()) {
                for (int i = 0; i < adCodeProvLst.size(); i++) {
                    final Integer provAdcode = adCodeProvLst.get(i);
                    final Area provArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, provAdcode);
                    if (provArea != null) {
                        final ProvDataInfo provDataInfo = convertProvData(provArea);

                        // 一级列表非省份数据（直辖市+省辖市）
                        if(provDataInfo.getAreaType() == 2 || provDataInfo.getAreaType() == 3) {
                            // 获取一级城市数据包的下载状态信息(模糊搜索)
                            provDataInfo.setDownLoadInfo(getCityDownLoadInfo(provDataInfo.getAdcode()));
                        }

                        final ArrayList<CityDataInfo> cityBeanList = new ArrayList<>();
                        final ArrayList<Integer> subCityAdcodeList = provArea.vecLowerAdcodeList;
                        if (subCityAdcodeList.size() == 0) {
                            final CityDataInfo cityDataInfo = new CityDataInfo();
                            cityDataInfo.setName(provDataInfo.getName());
                            cityDataInfo.setAreaType(provDataInfo.getAreaType());
                            cityDataInfo.setAdcode(provDataInfo.getAdcode());
                            cityDataInfo.setJianPin(provDataInfo.getJianPin());
                            cityDataInfo.setPinYin(provDataInfo.getPinYin());
                            cityDataInfo.setDownLoadInfo(getCityDownLoadInfo(provDataInfo.getAdcode()));
                            // 获取对应省份下的城市列表
                            cityBeanList.add(cityDataInfo);
                        } else {
                            if (null != subCityAdcodeList && !subCityAdcodeList.isEmpty()) {
                                for (int j = 0; j < subCityAdcodeList.size(); j++) {
                                    final Integer cityAdcode = subCityAdcodeList.get(j);
                                    final Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, cityAdcode);
                                    if (cityArea != null) {
                                        // 获取对应省份下的城市列表
                                        cityBeanList.add(getCityInfo(cityArea.adcode));
                                        provDataInfo.setCityInfoList(cityBeanList); // 省份下所有城市列表
                                    }
                                }
                            }
                        }

//                        cityBeanList.add(0, convertAllCityData(cityBeanList, provDataInfo.getAdcode())); // 增加“全省地图”item
                        provDataInfo.setCityInfoList(cityBeanList); // 省份下所有城市列表

                        provinceBeanList.add(provDataInfo);
                    }

                }
            }
            return provinceBeanList;
        }
        Logger.d(TAG, "searchAdCode: null");
        return null;
    }

    /**
     * 通过搜索关键字获取已下载的行政区域adcode列表
     * 该方法对外提供使用
     *
     * @param strKey
     * @return 返回匹配到关键的已下载信息
     */
    public ArrayList<ProvDataInfo> searchDownLoaded(final String strKey) {
        if (mMapDataService != null) {
            //已下载数据集合（城市与省份同级）
            final  ArrayList<ProvDataInfo> provinceBeanList = new ArrayList<>();
            // 获取adCode列表
            final ArrayList<Integer> adCodeProvLst = mMapDataService.searchAdcode(strKey);
            if (null != adCodeProvLst && !adCodeProvLst.isEmpty()) {
                for (int i = 0; i < adCodeProvLst.size(); i++) {
                    final Integer provAdcode = adCodeProvLst.get(i);
                    final Area provArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, provAdcode);
                    if (provArea != null) {
                        final ProvDataInfo provDataInfo = convertProvData(provArea);

                        // 一级列表非省份数据（直辖市+省辖市）
                        if(provDataInfo.getAreaType() == 2 || provDataInfo.getAreaType() == 3) {
                            // 获取一级城市数据包的下载状态信息(模糊搜索)
                            provDataInfo.setDownLoadInfo(getCityDownLoadInfo(provDataInfo.getAdcode()));
                            if (provDataInfo.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_SUCCESS) {
                                provinceBeanList.add(provDataInfo); // 匹配到的城市数据为已下载的直辖市和省辖市集合
                            }
                        }

                        final ArrayList<CityDataInfo> cityBeanList = new ArrayList<>();
                        final ArrayList<Integer> subCityAdcodeList = provArea.vecLowerAdcodeList;
                        if (null != subCityAdcodeList && !subCityAdcodeList.isEmpty()) {
                            for (int j = 0; j < subCityAdcodeList.size() - 1; j++) {
                                final Integer cityAdcode = subCityAdcodeList.get(j);
                                final Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, cityAdcode);
                                if (cityArea != null) {
                                    final CityDataInfo cityDataInfo = getCityInfo(cityArea.adcode);
                                    // 获取对应省份下已下载的城市列表
                                    if (cityDataInfo.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_SUCCESS) {
                                        cityBeanList.add(cityDataInfo);
                                    }
                                    provDataInfo.setCityInfoList(cityBeanList); // 省份下所有已下载城市列表
                                }

                            }
                        }
                        if (provDataInfo.getCityInfoList() != null && !provDataInfo.getCityInfoList().isEmpty()) {
                            provinceBeanList.add(provDataInfo); // 匹配到的省份下面存在已下载的城市数据
                        }

                    }
                }
            }
            return provinceBeanList;
        }
        Logger.d(TAG, "searchDownloaded: null");
        return null;
    }

    /**
     * 通过搜索城市关键字获取行政区域adcode列表
     * @param strKey
     * @return 返回对应的行政区域信息
     */
    public int searchCityAdCode(final String strKey) {
        if (mMapDataService != null) {
            final ArrayList<Integer> adcode = mMapDataService.searchAdcode(strKey);
            if (null != adcode && !adcode.isEmpty()) {
                return adcode.get(0);
            }
        }
        Logger.d(TAG, "searchAdCode: null");
        return 0;
    }

    /**
     * 获取区域扩展信息
     * @param adminCode
     * @return 返回区域扩展信息
     */
    @SuppressLint("WrongConstant")
    public AreaExtraInfoBean getAreaExtraInfo(final AdminCodeBean adminCode) {
        if (mMapDataService != null && adminCode != null) {
            final AreaExtraInfoBean areaExtraInfoBean = new AreaExtraInfoBean();
            final AreaExtraInfo areaExtraInfo = mMapDataService.getAreaExtraInfo(new
                    AdminCode(adminCode.getEuRegionCode(), adminCode.getnCityAdCode(), adminCode.getnAdCode()));
            if (areaExtraInfo != null) {
                areaExtraInfoBean.setCityName(areaExtraInfo.cityName);
                areaExtraInfoBean.setTownName(areaExtraInfo.townName);
                areaExtraInfoBean.setProvName(areaExtraInfo.provName);
                areaExtraInfoBean.setStAdCode(new AdminCodeBean(areaExtraInfo.stAdCode.euRegionCode,
                        areaExtraInfo.stAdCode.nCityAdCode, areaExtraInfo.stAdCode.nAdCode));
                areaExtraInfoBean.setStCenterPoint(new AdMapPointBean(
                        areaExtraInfo.stCenterPoint.nLon, areaExtraInfo.stCenterPoint.nLat,
                        areaExtraInfo.stCenterPoint.nZlevel));
            }
            return areaExtraInfoBean;
        }
        Logger.d(TAG, "getAreaExtraInfo: null");
        return null;
    }

    /**
     * 获取离线数据版本号
     * @param adCode  当前城市的行政编码
     * @return 返回离线数据版本信息
     */
    public String getDataFileVersion(final int adCode) {
        if (mMapDataService != null) { // 当前城市无离线数据，显示默认版本号0_000000
            // 通过adCode获取各地图数据文件版本号
            final String m1Version = mMapDataService.getDataFileVersion(adCode, MapDataFileType.MAP_DATA_TYPE_FILE_MAP);
            final String m2Version = mMapDataService.getDataFileVersion(adCode, MapDataFileType.MAP_DATA_TYPE_FILE_ROUTE);
            final String m3Version = mMapDataService.getDataFileVersion(adCode, MapDataFileType.MAP_DATA_TYPE_FILE_POI);
            final String m4_proVersion = mMapDataService.getDataFileVersion(adCode, MapDataFileType.MAP_DATA_TYPE_FILE_3D);
            final String m5aVersion = mMapDataService.getDataFileVersion(adCode, MapDataFileType.MAP_DATA_TYPE_FILE_JV);
            final String m5bVersion = mMapDataService.getDataFileVersion(adCode, MapDataFileType.MAP_DATA_TYPE_FILE_JVLINK);
            if (null != m1Version && !m1Version.isEmpty()) { // m1.ans文件存在,在使用m1的版本号做为基准
                mDataVersion = m1Version + mSpaceStr + mOneStr;
                if (m1Version.equals(m2Version)) {
                    mDataVersion = mDataVersion + mOneStr;
                } else {
                    mDataVersion = mDataVersion + mZeroStr;
                }
                if (m1Version.equals(m3Version)) { // 依次判断m3、m4_pro、m5a、m5b
                    mDataVersion = mDataVersion + mOneStr;
                } else {
                    mDataVersion = mDataVersion + mZeroStr;
                }
                if (m1Version.equals(m4_proVersion)) {
                    mDataVersion = mDataVersion + mOneStr;
                } else {
                    mDataVersion = mDataVersion + mZeroStr;
                }
                if (m1Version.equals(m5aVersion)) {
                    mDataVersion = mDataVersion + mOneStr;
                } else {
                    mDataVersion = mDataVersion + mZeroStr;
                }
                if (m1Version.equals(m5bVersion)) {
                    mDataVersion = mDataVersion + mOneStr;
                } else {
                    mDataVersion = mDataVersion + mZeroStr;
                }
            } else if (null != m2Version && !m2Version.isEmpty()) {  // m1.ans文件不存在,在使用m2的版本号做为基准
                mDataVersion = m2Version + mSpaceStr + mOneStr;
                if (m2Version.equals(m3Version)) {
                    mDataVersion = mDataVersion + mOneStr;
                } else {
                    mDataVersion = mDataVersion + mZeroStr;
                }
                if (m2Version.equals(m4_proVersion)) {
                    mDataVersion = mDataVersion + mOneStr;
                } else {
                    mDataVersion = mDataVersion + mZeroStr;
                }
                if (m2Version.equals(m5aVersion)) {
                    mDataVersion = mDataVersion + mOneStr;
                } else {
                    mDataVersion = mDataVersion + mZeroStr;
                }
                if (m2Version.equals(m5bVersion)) {
                    mDataVersion = mDataVersion + mOneStr;
                } else {
                    mDataVersion = mDataVersion + mZeroStr;
                }
            } else if (null != m3Version && !m3Version.isEmpty()) {  // m2.ans文件不存在,在使用m3的版本号做为基准
                mDataVersion = m3Version + mSpaceStr + mOneStr;
                if (m3Version.equals(m4_proVersion)) {
                    mDataVersion = mDataVersion + mOneStr;
                } else {
                    mDataVersion = mDataVersion + mZeroStr;
                }
                if (m3Version.equals(m5aVersion)) {
                    mDataVersion = mDataVersion + mOneStr;
                } else {
                    mDataVersion = mDataVersion + mZeroStr;
                }
                if (m3Version.equals(m5bVersion)) {
                    mDataVersion = mDataVersion + mOneStr;
                } else {
                    mDataVersion = mDataVersion + mZeroStr;
                }
            } else if (null != m4_proVersion && !m4_proVersion.isEmpty()) {  // m3.ans文件不存在,在使用m4的版本号做为基准
                mDataVersion = m4_proVersion + mSpaceStr + mOneStr;
                if (m4_proVersion.equals(m5aVersion)) {
                    mDataVersion = mDataVersion + mOneStr;
                } else {
                    mDataVersion = mDataVersion + mZeroStr;
                }
                if (m4_proVersion.equals(m5bVersion)) {
                    mDataVersion = mDataVersion + mOneStr;
                } else {
                    mDataVersion = mDataVersion + mZeroStr;
                }
            } else if (null != m5aVersion && !m5aVersion.isEmpty()) {  // m4.ans文件不存在,在使用m5a的版本号做为基准
                mDataVersion = m5aVersion + mSpaceStr + mOneStr;
                if (m5aVersion.equals(m5bVersion)) {
                    mDataVersion = mDataVersion + mOneStr;
                } else {
                    mDataVersion = mDataVersion + mZeroStr;
                }
            } else if (null != m5bVersion && !m5bVersion.isEmpty()) {  // m5a.ans文件不存在,在使用m5b的版本号做为基准
                mDataVersion = m5bVersion + mSpaceStr + mOneStr;
            }
        }
        return mDataVersion;
    }

    /**
     * 通过adCode获取附近推荐城市信息
     * @param adCode
     * @return 返回附近推荐城市信息
     */
    public ArrayList<CityDataInfo> getNearAdCodeList(final int adCode) {
        if (mMapDataService != null) {
            final Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, adCode);
            if (cityArea == null) {
                Logger.e(TAG, "city area is null");
                return null;
            }
            final ArrayList<Integer> vecNearAdCodeList = cityArea.vecNearAdcodeList;
            final ArrayList<CityDataInfo> nearAdCodeList = new ArrayList<>();
            if (null != vecNearAdCodeList && !vecNearAdCodeList.isEmpty()) {
                for (int i = 0; i < vecNearAdCodeList.size(); i++) {
                    final  Integer cityAdCode = vecNearAdCodeList.get(i);
                    final Area area = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, cityAdCode);
                    if (area != null) {
                        nearAdCodeList.add(getCityInfo(area.adcode));
                    }
                }
            }
            Logger.d(TAG, "getNearAdCodeList: nearAdCodeList = " + GsonUtils.toJson(nearAdCodeList));
            return nearAdCodeList;
        }
        Logger.d(TAG, "getNearAdCodeList: null");
        return null;
    }

    @Override
    public void onInit(final int downLoadMode, final int dataType, final int opCode) {
        Logger.d(TAG, "mMapDataService onInit: downLoadMode = " + downLoadMode
                + " dataType = " + dataType + " opCode = " + opCode);
        if (opCode == Service.ErrorCodeOK) {
            // 初始化成功，继续操作
            Logger.d(TAG, "MapDataService 初始化成功");
        } else {
            // 初始化失败，其他处理
            Logger.d(TAG, "MapDataService 初始化失败");
        }
    }

    /**
     * 发起云端数据列表检测需要在初始化观察者IDataInitObserver的回调函数OnInit触发后发起
     */
    public void requestDataListCheck() {
        if (mMapDataService != null) {
            mMapDataService.requestDataListCheck(DownLoadMode.DOWNLOAD_MODE_NET, "", this);
        }
    }

    @Override
    public void onRequestDataListCheck(final int downLoadMode, final int dataType, final int opCode) {
        // 若有需要操作UI，请转UI线程
        Logger.d(TAG, "onRequestDataListCheck : downLoadMode = " + downLoadMode
                + " dataType = " + dataType + " opCode = " + opCode);
        if (opCode == Service.ErrorCodeOK) {
            // 初始化成功，继续操作
            Logger.d(TAG, "发起云端数据列表检测成功");
            mCommonManager.insertOrReplace(UserDataCode.SETTING_MESSAGE_CHECK_MAP_DATA_TIME, String.valueOf(System.currentTimeMillis()));
        } else {
            // 初始化失败，其他处理
            Logger.d(TAG, "发起云端数据列表检测失败");
        }

        if (ConvertUtils.isEmpty(mapDataResultObserverHashtable)) {
            return;
        }
        for (MapDataAdapterCallBack callBack : mapDataResultObserverHashtable.values()) {
            if (callBack == null) {
                continue;
            }
            callBack.onRequestCheckSuccess(downLoadMode, dataType, opCode);
        }
    }

    /**
     * 下载操作回调
     *
     * @param downLoadMode
     * @param dataType
     * @param opType
     * @param opreatedIdList
     */
    @Override
    public void onOperated(final int downLoadMode, final int dataType, final int opType, final ArrayList<Integer> opreatedIdList) {
        Logger.d(TAG, "onOperated: downLoadMode = " + downLoadMode + TAG_DATA_TYPE + dataType +
                "; opType = " + opType + "; opreatedIdList = " + GsonUtils.toJson(opreatedIdList));
    }

    /**
     * 下载状态回调
     *
     * @param downLoadMode 下载模式
     * @param dataType 数据类型。 当dataType参数值为DATA_TYPE_MAP=0时，id参数为城市行政编码adcode值。
     * @param id  数据id
     * @param taskCode  任务状态
     * @param opCode 操作状态码
     */
    @SuppressLint("WrongConstant")
    @Override
    public void onDownLoadStatus(final int downLoadMode, final int dataType, final int id, final int taskCode, final int opCode) {
        Logger.d(TAG, "onDownLoadStatus: downLoadMode = " + downLoadMode + TAG_DATA_TYPE + dataType +
                TAG_ID + id + "; taskCode = " + taskCode + "; opCode = " + opCode);

        ThreadManager.getInstance().postUi(() -> {
            /** OperationErrCode 处理 */
            if (opCode == UserDataCode.OPT_NET_DISCONNECT) {    // 无网络
                // 提示无网络链接toast
                ToastUtils.Companion.getInstance().showCustomToastView("无网络连接，请检查网络后重试");
            } else if (opCode == UserDataCode.OPT_DOWNLOAD_NET_ERROR) { // 网络异常
                // 提示网络错误toast
                ToastUtils.Companion.getInstance().showCustomToastView("网络异常，请检查网络后重试");
            } else if (opCode == UserDataCode.OPT_NO_SPACE_LEFTED) { // 磁盘空间不足
                // 内存空间不足toast
                ToastUtils.Companion.getInstance().showCustomToastView("存储空间不足，已暂停下载");
                // 暂停获取正在下载的城市数据
//                final ArrayList<Integer> adCodeList = mMapDataService.getWorkingQueueAdcodeList(DownLoadMode.DOWNLOAD_MODE_NET);
//                mMapDataService.operate(DownLoadMode.DOWNLOAD_MODE_NET, UserDataCode.OPERATION_TYPE_PAUSE, adCodeList);
            } else if (opCode == UserDataCode.OPT_SPACE_NOT_ENOUGHT) { // 存储空间可能不足，是否继续下载？
                // 内存空间不足toast
                ToastUtils.Companion.getInstance().showCustomToastView("存储空间不足，已暂停下载");
//                final ArrayList<Integer> adCodeList = mMapDataService.getWorkingQueueAdcodeList(DownLoadMode.DOWNLOAD_MODE_NET);
//                mMapDataService.operate(DownLoadMode.DOWNLOAD_MODE_NET, UserDataCode.OPERATION_TYPE_PAUSE, adCodeList);
            } else if (opCode == UserDataCode.OPT_ERROR) { // 操作异常

            }
        });

        /** 通知全部数据变更,获取城市数据信息更新UI控件文案信息 */
        updateDownloadStatus(id); // 备注：如果不想全部更新，可以获取只更新参数 id 中的城市数据项
    }

    /**
     * 下载进度百分比回调
     *
     * @param downLoadMode
     * @param dataType
     * @param id
     * @param percentType 0-表示下载进度，1-表示解压进度。
     * @param percent
     */
    @Override
    public void onPercent(final int downLoadMode, final int dataType, final int id, final int percentType, final float percent) {
        Logger.d(TAG, "onPercent: downLoadMode = " + downLoadMode + TAG_DATA_TYPE + dataType + "; percentType = " +
                percentType + TAG_ID + id + "; percent = " + percent);
        updateDownloadStatus(id);
    }

    /**
     * 通知下载回调状态
     * @param id
     */
    private void updateDownloadStatus(final int id) {
        //更新城市下载信息
        final Area area = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, id);
        if (area == null) {
            Logger.e(TAG, "area is null");
            return;
        }
        Logger.d(TAG, "onDownLoadStatus: area = " + GsonUtils.toJson(area));

        CityDataInfo cityDataInfo = getCityInfo(area.adcode);

        if (area.areaType == 0) { // 基础包
            cityDataInfo.setUpperAdcode(0);
        } else if (area.areaType == 2) { // 直辖市
            cityDataInfo.setUpperAdcode(2);
        } else if (area.areaType == 4) { // 特别行政区
            cityDataInfo.setUpperAdcode(4);
        }

        Logger.d(TAG, "onDownLoadStatus: cityDataInfo = " + GsonUtils.toJson(cityDataInfo));
        if (ConvertUtils.isEmpty(mapDataResultObserverHashtable)) {
            return;
        }
        for (MapDataAdapterCallBack callBack : mapDataResultObserverHashtable.values()) {
            if (callBack == null) {
                continue;
            }
            callBack.onDownLoadStatus(cityDataInfo);
        }
    }

    private ProvDataInfo getProvDataInfo(final Area area) {
        ProvDataInfo provDataInfo = new ProvDataInfo();
        if (area.areaType == 0) { // 基础包
            provDataInfo.setName("基础包功能包");
            provDataInfo.setAreaType(0);
            provDataInfo.setAdcode(0);
            provDataInfo.setJianPin("jcgnb");
            provDataInfo.setPinYin("jichugongnengbao");
            final ArrayList<CityDataInfo> cityDataInfos = new ArrayList<>();
            cityDataInfos.add(getCountryData());
            provDataInfo.setCityInfoList(cityDataInfos);
        } else if (area.areaType == 2) { // 直辖市
            provDataInfo = getDirectDataInfo();
        } else if (area.areaType == 4) { // 特别行政区
            provDataInfo = getSpecialDataInfo();
        } else { // 普通省份
            final ProvinceInfo info = mMapDataService.getProvinceInfo(area.upperAdcode);
            provDataInfo.setName(info.provName);
            provDataInfo.setAreaType(info.provLevel);
            provDataInfo.setAdcode(info.provAdcode);
            provDataInfo.setJianPin(info.provInitial);
            provDataInfo.setPinYin(info.provPinyin);

            final ArrayList<CityDataInfo> cityDataInfos = new ArrayList<>();
            for (int i = 0; i < info.cityInfoList.size(); i++) {
                final Integer cityAdcode = info.cityInfoList.get(i).cityAdcode;
                final Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, cityAdcode);
                cityDataInfos.add(convertCityData(cityArea));
            }
            provDataInfo.setCityInfoList(cityDataInfos);
        }
        return provDataInfo;
    }

    /**
     * 数据包操作
     * @param opType
     * @param adCodeDiyLst
     */
    public void operate(final int opType, final ArrayList<Integer> adCodeDiyLst) {
        Logger.d(TAG, "operate: opType = " + opType + ", adCodeDiyLst = " + GsonUtils.toJson(adCodeDiyLst));
        boolean isNetConnected = NetWorkUtils.Companion.getInstance().checkNetwork();
        if (!isNetConnected) {
            // 提示无网络链接toast
            ToastUtils.Companion.getInstance().showCustomToastView("无网络连接，请检查网络后重试");
            return;
        }
        ThreadManager.getInstance().postDelay(() -> {
            // 如果当前城市的数据包正在解压，则暂停操作不会立即生效，需等待解压完成。
            if (mMapDataService != null) {
                mMapDataService.operate(DownLoadMode.DOWNLOAD_MODE_NET, opType, adCodeDiyLst);
            }
        }, 0);
    }

    /**
     * 引擎数据融合状态信息回调
     *
     * @param mergedStatusInfo
     */
    @Override
    public void onMergedStatusInfo(final MergedStatusInfo mergedStatusInfo) {
        Logger.d(TAG, "onMergedStatusInfo: mergedStatusInfo = " + GsonUtils.toJson(mergedStatusInfo));
        if (ConvertUtils.isEmpty(mapDataResultObserverHashtable)) {
            return;
        }
        for (MapDataAdapterCallBack callBack : mapDataResultObserverHashtable.values()) {
            if (callBack == null) {
                continue;
            }
            final MergedStatusBean mergedStatusBean = new MergedStatusBean();
            mergedStatusBean.setMergedSate(mergedStatusInfo.bMergedSate);
            mergedStatusBean.setAdcode(mergedStatusInfo.adcode);
            mergedStatusBean.setCityName(mergedStatusInfo.cityName);
            mergedStatusBean.setPackageType(mergedStatusInfo.packageType);
            mergedStatusBean.setErrType(mergedStatusInfo.errType);
            mergedStatusBean.setUpdateType(mergedStatusInfo.updateType);
            mergedStatusBean.setUrl(mergedStatusInfo.url);
            mergedStatusBean.setErrTypeDetill(mergedStatusInfo.errTypeDetill);
            mergedStatusBean.setCostMergeTime(mergedStatusInfo.costMergeTime);
            callBack.onMergedStatusInfo(mergedStatusBean);
        }
    }

    /**
     * 数据异常通知
     *
     * @param downLoadMode
     * @param dataType
     * @param id
     * @param errType
     * @param errMsg
     */
    @Override
    public void onErrorNotify(final int downLoadMode, final int dataType, final int id, final int errType, final String errMsg) {
        Logger.d(TAG, "onErrorNotify: downLoadMode = " + downLoadMode + TAG_DATA_TYPE + dataType +
                TAG_ID + id + "; errType = " + errType + "; errMsg = " + errMsg);
        if (ConvertUtils.isEmpty(mapDataResultObserverHashtable)) {
            return;
        }
        for (MapDataAdapterCallBack callBack : mapDataResultObserverHashtable.values()) {
            if (callBack == null) {
                continue;
            }
            callBack.onErrorNotify(downLoadMode, dataType, id, errType, errMsg);
        }
    }

    /**
     * 异常数据清除回调通知
     *
     * @param downLoadMode
     * @param dataType
     * @param id
     * @param opCode
     */
    @Override
    public void onDeleteErrorData(final int downLoadMode, final int dataType, final int id, final int opCode) {
        Logger.d(TAG, "onDeleteErrorData: downLoadMode = " + downLoadMode + TAG_DATA_TYPE + dataType +
                TAG_ID + id + "; opCode = " + opCode);
        if (ConvertUtils.isEmpty(mapDataResultObserverHashtable)) {
            return;
        }
        for (MapDataAdapterCallBack callBack : mapDataResultObserverHashtable.values()) {
            if (callBack == null) {
                continue;
            }
            callBack.onDeleteErrorData(downLoadMode, dataType, id, opCode);
        }
    }

}
