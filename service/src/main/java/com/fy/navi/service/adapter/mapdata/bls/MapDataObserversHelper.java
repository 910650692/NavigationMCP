package com.fy.navi.service.adapter.mapdata.bls;

import android.annotation.SuppressLint;
import android.view.ViewGroup;

import com.android.utils.ConvertUtils;
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
import com.autonavi.gbl.data.model.OperationType;
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

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

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
            return convertCityData(cityArea);
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
                    directCityList.add(convertCityData(cityArea));
                }
                provDataInfo.setName("直辖市");
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
                    directCityList.add(convertCityData(cityArea));
                }
                provDataInfo.setName("特别行政区");
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

            // 2，获取省份adCode列表（按拼音排序）
            final ArrayList<Integer> adCodeProvLst = mMapDataService.getAdcodeList(DownLoadMode.DOWNLOAD_MODE_NET, AreaType.AREA_TYPE_PROV);
            final ArrayList<Integer> allProvinceList = new ArrayList<>(adCodeProvLst); // 添加一级省份对应信息
            if (!allProvinceList.isEmpty()) {
                for (int i = 0; i < allProvinceList.size(); i++) {
                    final Integer provAdcode = allProvinceList.get(i);
                    final Area provArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, provAdcode);
                    final ProvDataInfo provDataInfo = convertProvData(provArea);
                    // 获取对应省份下的城市列表
                    final ArrayList<CityDataInfo> cityBeanList = new ArrayList<>();
                    final ArrayList<Integer> subCityAdcodeList = provArea.vecLowerAdcodeList;
                    if (null != subCityAdcodeList && !subCityAdcodeList.isEmpty()) {
                        for (int j = 0; j < subCityAdcodeList.size() - 1; j++) {
                            final Integer cityAdcode = subCityAdcodeList.get(j);
                            final  Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, cityAdcode);
                            cityBeanList.add(convertCityData(cityArea));
                        }
                    }
                    provDataInfo.setCityInfoList(cityBeanList);
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
                if (downloadItem.taskState == UserDataCode.TASK_STATUS_CODE_SUCCESS) {
                    cityDownLoadBean.setStatusTip("已下载");
                } else {
                    cityDownLoadBean.setStatusTip(switchTaskStatusCodeToString(downloadItem.bIsDataUsed,
                            downloadItem.taskState, downloadItem.IsCompltelyHighVer));
                }
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
                cityInfo.setName(info.cityName);
                cityInfo.setAreaType( info.cityLevel);
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
     * @param isDataUsed
     * @param taskCode
     * @param isCompltelyHighVer
     *
     * @return 返回下载提示文字
     */
    private String switchTaskStatusCodeToString(final boolean isDataUsed, final int taskCode, final boolean isCompltelyHighVer) {
        String desc = new String("");
        switch (taskCode) {
            case TaskStatusCode.TASK_STATUS_CODE_READY:
                if (isDataUsed) {
                    desc = "待更新";
                    if (isCompltelyHighVer) {
                        // 全量更新，数据包待更新的大小用CityDownLoadItem.nFullZipSize字段值来显示
                    } else {
                        // 增量更新，数据包待更新的大小用CityDownLoadItem.nZipSize字段值来显示
                    }
                } else {
                    desc = "下载";
                    // 待下载，数据包待下载的大小用CityDownLoadItem.nFullZipSize字段值来显示
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
                if(isDataUsed) {
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
    public ArrayList<CityDataInfo> getWorkingList() {
        final ArrayList<CityDataInfo> cityBeanList = new ArrayList<>();
        if (mMapDataService != null) {
            final ArrayList<Integer> adCodeList = mMapDataService.getWorkingQueueAdcodeList(DownLoadMode.DOWNLOAD_MODE_NET);
            if (null != adCodeList && !adCodeList.isEmpty()) {
                for (int i = 0; i < adCodeList.size(); i++) {
                    final Integer cityAdCode = adCodeList.get(i);
                    final Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, cityAdCode);
                    final CityDataInfo  cityDataInfo = convertCityData(cityArea);
                    if (cityDataInfo.getDownLoadInfo().getTaskState() == TaskStatusCode.TASK_STATUS_CODE_WAITING // 等待中
                            || cityDataInfo.getDownLoadInfo().getTaskState() == TaskStatusCode.TASK_STATUS_CODE_DOING
                            || cityDataInfo.getDownLoadInfo().getTaskState() == TaskStatusCode.TASK_STATUS_CODE_DONE // 下载中
                            || cityDataInfo.getDownLoadInfo().getTaskState() == TaskStatusCode.TASK_STATUS_CODE_PAUSE) {  // 暂停
                        // 下载中/更新中列表  等待中、下载中、暂停
                        cityBeanList.add(cityDataInfo);
                    }
                }
            }
        }
        return cityBeanList;
    }

    /**
     * 获取已下载状态下的所有城市adCode列表
     * @return 返回已下载的信息
     */
    public ArrayList<CityDataInfo> getWorkedList() {
        final ArrayList<CityDataInfo> downloadedList = new ArrayList<>();
        final ArrayList<CityDataInfo> dataInfos = getCityInfoList();
        if (null != dataInfos && !dataInfos.isEmpty()) {
            for (CityDataInfo info : dataInfos) {
                // 获取城市数据包的下载状态信息
                info.setDownLoadInfo(getCityDownLoadInfo(info.getAdcode()));
                if (info.getDownLoadInfo().getTaskState() == TaskStatusCode.TASK_STATUS_CODE_SUCCESS) {
                    // 已下载列表
                    downloadedList.add(info);
                }
            }
        }
        return downloadedList;
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
        final ArrayList<Integer> downLoadAdcodeList =mMapDataService.getDownLoadAdcodeList();
        Logger.d(TAG, "getDownLoadAdCodeList: downLoadAdcodeList" + GsonUtils.toJson(downLoadAdcodeList));

        if (null != downLoadAdcodeList && !downLoadAdcodeList.isEmpty()) {

            //获取已下载省份code列表
            final ArrayList<Integer> provAdcodeList = new ArrayList<>();
            for (int i = 0; i < downLoadAdcodeList.size(); i++) {
                final Integer downLoadAdCode = downLoadAdcodeList.get(i);
                final Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, downLoadAdCode);
                if (!provAdcodeList.contains(cityArea.upperAdcode)) {
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
                    provDataInfo.setAdcode(0);
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
                        final CityDataInfo cityDataInfo = convertCityData(cityArea);
                        if (cityDataInfo.getUpperAdcode() == provDataInfo.getAdcode()) {
                            cityBeanList.add(cityDataInfo);
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
                                    // 获取对应省份下的城市列表
                                    cityBeanList.add(convertCityData(cityArea));
                                    provDataInfo.setCityInfoList(cityBeanList); // 省份下所有城市列表
                                }
                            }
                        }

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
                                final CityDataInfo cityDataInfo = convertCityData(cityArea);
                                // 获取对应省份下已下载的城市列表
                                if (cityDataInfo.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_SUCCESS) {
                                    cityBeanList.add(cityDataInfo);
                                }
                                provDataInfo.setCityInfoList(cityBeanList); // 省份下所有已下载城市列表
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
     * 获取所有城市信息列表
     * @return 返回所有城市信息
     */
    private ArrayList<CityDataInfo> getCityInfoList() {
        if (mMapDataService != null) {
            // 全部城市数据
            final ArrayList<CityDataInfo> cityDataInfos = new ArrayList<>();
            final ArrayList<CityItemInfo> cityItemInfos = mMapDataService.getCityInfoList();
            if (cityItemInfos == null) {
                return null;
            }
            for (CityItemInfo info : cityItemInfos) {
                final  CityDataInfo dataInfo = new CityDataInfo();
                dataInfo.setUpperAdcode(info.belongedProvince);
                dataInfo.setName(info.cityName);
                dataInfo.setAreaType(info.cityLevel);
                dataInfo.setAdcode(info.cityAdcode);
                dataInfo.setJianPin(info.initial);
                dataInfo.setPinYin(info.pinyin);
                cityDataInfos.add(dataInfo);
            }
            Logger.d(TAG, "getCityInfoList: cityDataInfos = " + GsonUtils.toJson(cityDataInfos));
            return cityDataInfos;
        }
        Logger.d(TAG, "getCityInfoList is null");
        return null;
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
                    AdminCode(adminCode.euRegionCode, adminCode.nCityAdCode, adminCode.nAdCode));
            if (areaExtraInfo != null) {
                areaExtraInfoBean.cityName = areaExtraInfo.cityName;
                areaExtraInfoBean.townName = areaExtraInfo.townName;
                areaExtraInfoBean.provName = areaExtraInfo.provName;
                areaExtraInfoBean.stAdCode = new AdminCodeBean(areaExtraInfo.stAdCode.euRegionCode,
                        areaExtraInfo.stAdCode.nCityAdCode, areaExtraInfo.stAdCode.nAdCode);
                areaExtraInfoBean.stCenterPoint = new AdMapPointBean(areaExtraInfo.stCenterPoint.nLon,
                        areaExtraInfo.stCenterPoint.nLat, areaExtraInfo.stCenterPoint.nZlevel);
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
            final ArrayList<Integer> vecNearAdCodeList = cityArea.vecNearAdcodeList;
            final ArrayList<CityDataInfo> nearAdCodeList = new ArrayList<>();
            if (null != vecNearAdCodeList && !vecNearAdCodeList.isEmpty()) {
                for (int i = 0; i < vecNearAdCodeList.size(); i++) {
                    final  Integer cityAdCode = vecNearAdCodeList.get(i);
                    final Area area = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, cityAdCode);
                    nearAdCodeList.add(convertCityData(area));
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
                final ArrayList<Integer> adCodeList = mMapDataService.getWorkingQueueAdcodeList(DownLoadMode.DOWNLOAD_MODE_NET);
                mMapDataService.operate(DownLoadMode.DOWNLOAD_MODE_NET, OperationType.OPERATION_TYPE_PAUSE, adCodeList);
            } else if (opCode == UserDataCode.OPT_SPACE_NOT_ENOUGHT) { // 存储空间可能不足，是否继续下载？
                // 内存空间不足toast
                ToastUtils.Companion.getInstance().showCustomToastView("存储空间不足，已暂停下载");
                final ArrayList<Integer> adCodeList = mMapDataService.getWorkingQueueAdcodeList(DownLoadMode.DOWNLOAD_MODE_NET);
                mMapDataService.operate(DownLoadMode.DOWNLOAD_MODE_NET, OperationType.OPERATION_TYPE_PAUSE, adCodeList);
            } else if (opCode == UserDataCode.OPT_ERROR) { // 操作异常

            }
        });

        /** 通知全部数据变更,获取城市数据信息更新UI控件文案信息 */
        updateDownloadStatus(id); // 备注：如果不想全部更新，可以获取只更更新参数 id 中的城市数据项
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
        //获取下载城市信息
        final List<CityDataInfo> cityList = new ArrayList<>();
        CityDataInfo cityDataInfo = new CityDataInfo();
        if (mMapDataService != null) {
            final Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, id);
            cityDataInfo = convertCityData(cityArea);
            cityList.add(cityDataInfo);
        }

        final ProvDataInfo provDataInfo = new ProvDataInfo();
        provDataInfo.setCityInfoList(cityList); // 省份下所有城市列表
        provDataInfo.setName(cityDataInfo.getName());
        provDataInfo.setAreaType(cityDataInfo.getAreaType());
        provDataInfo.setAdcode(cityDataInfo.getAdcode());
        provDataInfo.setJianPin(cityDataInfo.getJianPin());
        provDataInfo.setPinYin(cityDataInfo.getPinYin());
        provDataInfo.setDownLoadInfo(getCityDownLoadInfo(cityDataInfo.getAdcode()));

        if (ConvertUtils.isEmpty(mapDataResultObserverHashtable)) {
            return;
        }
        for (MapDataAdapterCallBack callBack : mapDataResultObserverHashtable.values()) {
            if (callBack == null) {
                continue;
            }
            callBack.onDownLoadStatus(provDataInfo);
        }
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
