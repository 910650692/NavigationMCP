package com.fy.navi.service.adapter.user.usertrack.bls;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.user.model.BehaviorDataType;
import com.autonavi.gbl.user.syncsdk.model.SyncMode;
import com.autonavi.gbl.user.usertrack.UserTrackService;
import com.autonavi.gbl.user.usertrack.model.BehaviorFileType;
import com.autonavi.gbl.user.usertrack.model.GpsTrackDepthInfo;
import com.autonavi.gbl.user.usertrack.model.GpsTrackPoint;
import com.autonavi.gbl.user.usertrack.model.SearchHistoryItem;
import com.autonavi.gbl.user.usertrack.observer.IGpsInfoGetter;
import com.autonavi.gbl.user.usertrack.observer.IUserTrackObserver;
import com.autonavi.gbl.util.model.ServiceInitStatus;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.user.usertrack.UserTrackAdapterCallBack;
import com.fy.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.fy.navi.service.define.user.usertrack.GpsTrackDepthBean;
import com.fy.navi.service.define.user.usertrack.SearchHistoryItemBean;
import com.fy.navi.service.greendao.history.History;
import com.fy.navi.service.greendao.history.HistoryManager;
import com.google.gson.Gson;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

/**
 * UserTrackService 辅助类.
 *
 * @Description Helper类只做对象及数据转换，不做原子能力调用
 * @Author fh
 * @date 2024/12/26
 */
public class UserTrackImplHelper implements IUserTrackObserver, IGpsInfoGetter {
    private static final String TAG = MapDefaultFinalTag.USER_TRACK_SERVICE_TAG;
    private final Hashtable<String, UserTrackAdapterCallBack> userTrackResultHashtable;
    private UserTrackService mUserTrackService;
    private HistoryManager historyManager;
    private ArrayList<DrivingRecordDataBean> guideDataBeans = new ArrayList<>();
    private ArrayList<DrivingRecordDataBean> cruiseDataBeans = new ArrayList<>();

    protected UserTrackImplHelper(UserTrackService userTrackService) {
        userTrackResultHashtable = new Hashtable<>();
        mUserTrackService = userTrackService;
        historyManager = HistoryManager.getInstance();
        historyManager.init();
    }

    public void registerCallBack(String key, UserTrackAdapterCallBack callBack) {
        userTrackResultHashtable.put(key, callBack);
    }

    public void unRegisterCallBack(String key) {
        userTrackResultHashtable.remove(key);
    }

    public void removeCallback() {
        userTrackResultHashtable.clear();
    }

    /**
     * 初始化服务
     */
    public void initUserTrackService() {
        //初始化参数待配置
        int userTrack = mUserTrackService.init(this);
        // 添加轨迹服务观察者
        mUserTrackService.addObserver(this);
    }

    /**
     * 获取搜索历史记录列表
     * @return
     */
    public ArrayList<SearchHistoryItemBean> getSearchHistory() {
        if (mUserTrackService == null) return null;
        if (mUserTrackService.getSearchHistory() == null) return null;
        ArrayList<SearchHistoryItemBean> historyItems = new ArrayList<>();
        for (SearchHistoryItem item : mUserTrackService.getSearchHistory()) {
            SearchHistoryItemBean bean = new SearchHistoryItemBean();
            GsonUtils.copyBean(item, bean);
            historyItems.add(bean);
        }
        return historyItems;
    }

    /**
     * 添加搜索历史记录
     * @return
     */
    public int addSearchHistory(SearchHistoryItemBean bean) {
        SearchHistoryItem item = new SearchHistoryItem();
        GsonUtils.copyBean(bean, item);
        if (mUserTrackService == null) return -1;
        return mUserTrackService.addSearchHistory(item, SyncMode.SyncModeNow);
    }

    /**
     * 删除搜索历史记录
     * @param name
     * @return
     */
    public int delSearchHistory(String name) {
        SearchHistoryItem item = new SearchHistoryItem();
        item.name = name; //  "肯德基";  删除只需要赋值名字字段
        if (mUserTrackService == null) return -1;
        int ret = mUserTrackService.delSearchHistory(item, SyncMode.SyncModeNow);
        Logger.i(TAG, "delSearchHistory ret = " + ret );
        return ret;
    }

    /**
     * 从sdk获取行程数据列表（同步数据到本地数据库）
     * @return
     */
    public void getDrivingRecordData() {
        if (mUserTrackService == null) return ;
        // 403  车机版只有 驾车轨迹(403) 生效
        int type = BehaviorDataType.BehaviorTypeTrailDriveForAuto;
        // 获取行程ID列表（已完结记录）
        int[] behaviorDataIds = mUserTrackService.getBehaviorDataIds(type);
        if (behaviorDataIds == null) return ;
        Logger.i(TAG, "behaviorDataIds -> " + GsonUtils.toJson(behaviorDataIds));

        // 通过id获取行为数据
        for (int num : behaviorDataIds) {
            // 通过id获取行程数据Json串
            String behaviorData = mUserTrackService.getBehaviorDataById(type, num);
            Logger.i(TAG, "behaviorData -> " + behaviorData);
            DrivingRecordDataBean dataBean = new Gson().fromJson(behaviorData, DrivingRecordDataBean.class);
            // 获取同步库轨迹文件
            String id = dataBean.getId();
            String filePath = mUserTrackService.getFilePath(type, id, BehaviorFileType.BehaviorFileTrail);// 车机版只有 轨迹文件(2) 生效
            dataBean.setFilePath(filePath);

            if (dataBean != null) {
                History history = new History();
                history.keyWord = "行程历史";
                history.poiId = dataBean.getId();// 数据ID
                history.startPoiName = dataBean.getStartPoiName(); // 设置起点
                history.endPoiName = dataBean.getEndPoiName(); // 设置终点
                history.startPoint = dataBean.getStartLocation();
                history.endPoint = dataBean.getEndLocation();
                history.runDistance = dataBean.getRunDistance();// 该行程行驶距离
                history.endTime = dataBean.getEndTime();  // 该行程完成时间
                history.rideRunType = dataBean.getRideRunType(); // 行程类型（导航/巡航）
                history.timeInterval = dataBean.getTimeInterval(); // 驾驶时长
                history.averageSpeed = dataBean.getAverageSpeed();// 平均速度
                history.maxSpeed = dataBean.getMaxSpeed();// 最快速度
                history.type = AutoMapConstant.SearchKeywordRecordKey.DRIVING_HISTORY_RECORD_KEY; // 该条记录类型
                historyManager.insertOrReplace(history);
            }
        }

    }

    /**
     * 获取导航行程历史数据
     * @return
     */
    public ArrayList<DrivingRecordDataBean> getDrivingRecordDataList() {
        guideDataBeans.clear();
        List<History> list = historyManager.getValueByType("行程历史");
        if (list != null && !list.isEmpty()) {
            for (History history : list) {
                DrivingRecordDataBean dataBean = new DrivingRecordDataBean();
                dataBean.setId(history.getPoiId()) ;// 数据ID
                dataBean.setStartPoiName(history.getStartPoiName()); ;// 设置起点
                dataBean.setEndPoiName(history.getEndPoiName()); ;// 设置终点
                dataBean.setStartLocation(history.getStartPoint()); ;// 数据ID
                dataBean.setEndLocation(history.getEndPoint()); ;// 数据ID
                dataBean.setRunDistance(history.getRunDistance()); ;// 该行程行驶距离
                dataBean.setEndTime(history.getEndTime()); ;// 该行程完成时间
                dataBean.setRideRunType(history.getRideRunType()); ;// 行程类型（导航/巡航）
                dataBean.setTimeInterval(history.getTimeInterval()); ;// 驾驶时长
                dataBean.setTimeInterval(history.getAverageSpeed()); ;// 平均速度
                dataBean.setMaxSpeed(history.getMaxSpeed()); ;// 最快速度
                if(history.getRideRunType() == 1) {
                    guideDataBeans.add(dataBean);
                }
            }
            Logger.i(TAG, "guideDataBeans -> " + GsonUtils.toJson(guideDataBeans));
        }
        return guideDataBeans;
    }

    /**
     * 获取巡航行程历史数据
     * @return
     */
    public ArrayList<DrivingRecordDataBean> getDrivingRecordCruiseDataList() {
        cruiseDataBeans.clear();
        List<History> list = historyManager.getValueByType("行程历史");
        if (list != null && !list.isEmpty()) {
            for (History history : list) {
                DrivingRecordDataBean dataBean = new DrivingRecordDataBean();
                dataBean.setId(history.getPoiId()) ;// 数据ID
                dataBean.setStartPoiName(history.getStartPoiName()); ;// 设置起点
                dataBean.setEndPoiName(history.getEndPoiName()); ;// 设置终点
                dataBean.setStartLocation(history.getStartPoint()); ;// 数据ID
                dataBean.setEndLocation(history.getEndPoint()); ;// 数据ID
                dataBean.setRunDistance(history.getRunDistance()); ;// 该行程行驶距离
                dataBean.setEndTime(history.getEndTime()); ;// 该行程完成时间
                dataBean.setRideRunType(history.getRideRunType()); ;// 行程类型（导航/巡航）
                dataBean.setTimeInterval(history.getTimeInterval()); ;// 驾驶时长
                dataBean.setTimeInterval(history.getAverageSpeed()); ;// 平均速度
                dataBean.setMaxSpeed(history.getMaxSpeed()); ;// 最快速度
                if(history.getRideRunType() == 0) {
                    cruiseDataBeans.add(dataBean);
                }
            }
            Logger.i(TAG, "cruiseDataBeans -> " + GsonUtils.toJson(cruiseDataBeans));
        }
        return cruiseDataBeans;
    }

    /**
     * IUserTrackObserver回调触发的同步事件
     * @param eventType
     * @param exCode
     */
    @Override
    public void notify(int eventType, int exCode) {
        Logger.i(TAG, "notify -> eventType = " + eventType + "; exCode = " + exCode);
        if (ConvertUtils.isEmpty(userTrackResultHashtable)) return;
        for (UserTrackAdapterCallBack callBack : userTrackResultHashtable.values()) {
            if (callBack == null) continue;
            callBack.notify(eventType, exCode);
        }
    }

    /**
     * IGpsInfoGetter回调获取GPS信息
     * @return
     */
    @Override
    public GpsTrackPoint getGpsTrackPoint() {
        // 从定位回调获取信息拼接GpsTrackPoint；若返回null,则无法生成轨迹点
        // TODO: 2024/12/26
        return null;
    }

    /**
     * 启动Gps打点，生成轨迹文件, 对应异步回调  通知开始结果
     * @param n32SuccessTag
     * @param psSavePath
     * @param psFileName
     */
    @Override
    public void onStartGpsTrack(int n32SuccessTag, String psSavePath, String psFileName) {
        if (ConvertUtils.isEmpty(userTrackResultHashtable)) return;
        for (UserTrackAdapterCallBack callBack : userTrackResultHashtable.values()) {
            if (callBack == null) continue;
            callBack.onStartGpsTrack(n32SuccessTag, psSavePath, psFileName);
        }
    }

    /**
     * 关闭Gps打点，异步回调返回深度信息。
     * 对应异步回调 onCloseGpsTrack 通知开始结果
     * @param n32SuccessTag
     * @param psSavePath
     * @param psFileName
     * @param depInfo
     */
    @Override
    public void onCloseGpsTrack(int n32SuccessTag, String psSavePath, String psFileName, GpsTrackDepthInfo depInfo) {
        if (ConvertUtils.isEmpty(userTrackResultHashtable)) return;
        for (UserTrackAdapterCallBack callBack : userTrackResultHashtable.values()) {
            if (callBack == null) continue;
            GpsTrackDepthBean info = new GpsTrackDepthBean();
            GsonUtils.copyBean(depInfo, info);
            callBack.onCloseGpsTrack(n32SuccessTag, psSavePath, psFileName, info);
        }
    }

    /**
     * 对应异步回调 轨迹信息 onGpsTrackDepInfo 通知开始结果
     * @param n32SuccessTag
     * @param psSavePath
     * @param psFileName
     * @param depInfo
     */
    @Override
    public void onGpsTrackDepInfo(int n32SuccessTag, String psSavePath, String psFileName, GpsTrackDepthInfo depInfo) {
        if (ConvertUtils.isEmpty(userTrackResultHashtable)) return;
        for (UserTrackAdapterCallBack callBack : userTrackResultHashtable.values()) {
            if (callBack == null) continue;
            GpsTrackDepthBean info = new GpsTrackDepthBean();
            GsonUtils.copyBean(depInfo, info);
            callBack.onGpsTrackDepInfo(n32SuccessTag, psSavePath, psFileName, info);
        }
    }

    /**
     * 校验账号数据服务状态.
     */
    protected void checkoutUserTrackServer() {
        if (ServiceInitStatus.ServiceNotInit == mUserTrackService.isInit()) {
            initUserTrackService();
        }
    }

    /**
     * 反初始化服务
     */
    public void unInitUserTrackService() {
        if (mUserTrackService != null) {
            removeCallback();
            if (ServiceInitStatus.ServiceInitDone != mUserTrackService.isInit()) {
                mUserTrackService.removeObserver(this);
                mUserTrackService.unInit();
            }
        }
    }

}
