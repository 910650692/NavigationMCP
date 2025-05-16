package com.fy.navi.service.logicpaket.user.usertrack;

import android.text.TextUtils;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.user.usertrack.UserTrackAdapter;
import com.fy.navi.service.adapter.user.usertrack.UserTrackAdapterCallBack;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.fy.navi.service.define.user.usertrack.GpsTrackDepthBean;
import com.fy.navi.service.define.user.usertrack.GpsTrackPointBean;
import com.fy.navi.service.define.user.usertrack.HistoryRouteItemBean;
import com.fy.navi.service.define.user.usertrack.SearchHistoryItemBean;
import com.fy.navi.service.greendao.CommonManager;
import com.fy.navi.service.greendao.history.History;
import com.fy.navi.service.greendao.history.HistoryManager;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;

import org.json.JSONException;
import org.json.JSONObject;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class UserTrackPackage implements UserTrackAdapterCallBack, SearchResultCallback {
    private static final String TAG = MapDefaultFinalTag.USER_TRACK_SERVICE_TAG;
    private final UserTrackAdapter mUserTrackAdapter;
    private Hashtable<String,UserTrackCallBack> mCallBacks;
    private final HistoryManager mHistoryManager;
    private final CommonManager mCommonManager;
    private boolean mIsNeedShowDialog = false;
    private final String mSPLIT = "_";
    private static final String KEY_X = "x";
    private static final String KEY_Y = "y";
    private static final String WRONG = "解析出错";

    private static final String DEFAULT_NAME = "地图选点";

    private JSONObject mJson = new JSONObject();
    private final History mHistory = new History();
    private int mReverseType = -1;
    private GpsTrackDepthBean mDepInfo;
    private String mPsFileName;

    private UserTrackPackage() {
        mUserTrackAdapter = UserTrackAdapter.getInstance();
        mHistoryManager = HistoryManager.getInstance();
        mHistoryManager.init();
        mCommonManager = CommonManager.getInstance();
        mCommonManager.init();
    }
    private final static int REVERSE_START = 8;
    private final static int REVERSE_FASTEST = 9;
    private final static int REVERSE_END = 10;

    /**
     * 初始化服务
     */
    public void initUserTrackService() {
        mCallBacks = new Hashtable<>();
        mUserTrackAdapter.initUserTrackService();
        mUserTrackAdapter.registerCallBack("UserTrackPackage", this);
        SearchPackage.getInstance().registerCallBack("UserTrackPackage", this);
    }

    /**
     * 注册回调
     * @param key 回调key
     * @param callback 回调
     */
    public synchronized void registerCallBack(final String key, final UserTrackCallBack callback) {
        if (callback != null && !mCallBacks.contains(callback)) {
            mCallBacks.put(key, callback);
        }
    }

    /**
     * 设置是否需要显示dialog
     * @param isNeedShowDialog true:需要显示 false:不需要显示
     */
    public void setIsNeedShowDialog(final boolean isNeedShowDialog) {
        mIsNeedShowDialog = isNeedShowDialog;
    }

    /**
     * 获取是否需要显示dialog
     * @return 是否需要显示dialog
     */
    public boolean getIsNeedShowDialog() {
        return mIsNeedShowDialog;
    }

    /**
     * 反初始化服务
     */
    public void unInitUserTrackService() {
        mUserTrackAdapter.unInitUserTrackService();
    }

    /**
     * 判断是否登录
     * @return 是否登录
     */
    public boolean isLogin() {
        return mUserTrackAdapter.isLogin();
    }

    /**
     * 获取搜索历史记录列表
     * @return 搜索历史记录列表
     */
    public ArrayList<SearchHistoryItemBean> getSearchHistory() {
        if (isLogin()) {
            return mUserTrackAdapter.getSearchHistory();
        }
        return getSearchHistoryFromDB();
    }

    /**
     * 从数据库获取搜索历史记录列表
     * @return 搜索历史记录列表
     */
    private ArrayList<SearchHistoryItemBean> getSearchHistoryFromDB() {
        final ArrayList<SearchHistoryItemBean> list = new ArrayList<>();
        final List<History> historyList = mHistoryManager.loadHistoryByPage(1,100);
        if (historyList != null && !historyList.isEmpty()) {
            for (History history : historyList) {
                final SearchHistoryItemBean item = new SearchHistoryItemBean();
                item.setName(history.getMKeyWord());
                list.add(item);
            }
        }
        return list;
    }

    /**
     * 添加搜索历史记录
     * @param item 搜索历史记录
     * @return 添加结果
     */
    public int addSearchHistory(final SearchHistoryItemBean item) {
        /*// 同步模式
        int mode = UserTrackModeType.SyncModeNow; // 立即同步
        // 2.1 搜索历史 关键字
        SearchHistoryItem item = new SearchHistoryItem();
        item.name = "高德";
        item.datatype_spec = "0"; // 可以自己定义 ICON 标记
        // 2.2 搜索历史 POI
        SearchHistoryItem item1 = new SearchHistoryItem();
        item1.name = "肯德基";
        item1.category = "50301";
        item1.datatype_spec = "1"; // 可以自己定义 ICON 标记*/
        if (isLogin()) {
            return mUserTrackAdapter.addSearchHistory(item);
        } else {
            addSearchHistoryToDB(item);
        }
        return 0;
    }

    /**
     * 添加搜索历史记录到数据库
     * @param item 搜索历史记录
     */
    private void addSearchHistoryToDB(final SearchHistoryItemBean item) {
        if (item == null) {
            return;
        }
        final History history = new History();
        history.setMKeyWord(item.getName());
        history.setMType(AutoMapConstant.SearchKeywordRecordKey.SEARCH_KEYWORD_RECORD_KEY);
        mHistoryManager.insertOrReplace(history);
    }

    /**
     * 删除搜索历史记录, 删除只需要赋值名字字段
     * @param name 搜索历史记录名称
     * @return 删除结果
     */
    public int delSearchHistory(final String name) {
        // name = "肯德基";
        if (isLogin()) {
            return mUserTrackAdapter.delSearchHistory(name);
        } else {
            return delHistoryFromDB(name);
        }
    }

    /**
     * 从数据库删除搜索历史记录
     * @param name 搜索历史记录名称
     * @return 删除结果
     */
    private int delHistoryFromDB(final String name) {
        if (TextUtils.isEmpty(name)) {
            return -1;
        }
        final List<History> history = mHistoryManager.getValueByType(name);
        if (history != null) {
            mHistoryManager.deleteValueByKey(name);
        }
        return 0;
    }

    /**
     * 删除所有搜索历史记录
     * @return 删除结果
     */
    public int clearSearchHistory(){
        if (isLogin()) {
            mUserTrackAdapter.clearSearchHistory();
        } else {
            mHistoryManager.deleteValueByKey(AutoMapConstant.SearchKeywordRecordKey.SEARCH_KEYWORD_RECORD_KEY);
        }
        return 0;
    }

    /**
     * 获取历史路线列表
     * @return 历史路线列表
     */
    public ArrayList<HistoryRouteItemBean> getHistoryRoute() {
        if (isLogin()) {
            return mUserTrackAdapter.getHistoryRoute();
        } else {
            return getHistoryRouteFromDB();
        }
    }

    /**
     * 从数据库获取历史路线列表
     * @return 历史路线列表
     */
    private ArrayList<HistoryRouteItemBean> getHistoryRouteFromDB() {
        final ArrayList<HistoryRouteItemBean> list = new ArrayList<>();
        final List<History> historyList = mHistoryManager.getValueByType(AutoMapConstant.SearchKeywordRecordKey.SEARCH_NAVI_RECORD_KEY);
        if (historyList != null && !historyList.isEmpty()) {
            for (History history : historyList) {
                final HistoryRouteItemBean item = new HistoryRouteItemBean();
                item.getToPoi().setName(history.getMKeyWord());
                item.getToPoi().setPoiId(history.getMPoiId());
                item.getToPoi().setPoiLoc(parseGeoPoint(history.getMEndPoint()));
                item.getFromPoi().setName(history.getMStartPoiName());
                item.getFromPoi().setPoiLoc(parseGeoPoint(history.getMStartPoint()));
                item.setType(history.getMType());
                item.setTime(history.getMUpdateTime());
                list.add(item);
            }
        }
        return list;
    }

    /**
     * 解析geoPoint字符串
     * @param geoPointString geoPoint字符串
     * @return GeoPoint
     */
    private GeoPoint parseGeoPoint(final String geoPointString) {
        final Pattern pattern = Pattern.compile("lon=([-\\d.]+), lat=([-\\d.]+)");
        final Matcher matcher = pattern.matcher(geoPointString);

        double lon = 0.0;
        double lat = 0.0;

        if (matcher.find()) {
            lon = Double.parseDouble(Objects.requireNonNull(matcher.group(1)));
            lat = Double.parseDouble(Objects.requireNonNull(matcher.group(2)));
        } else {
            Logger.e("parseGeoPoint: No match found for GeoPoint string: " + geoPointString);
        }
        return new GeoPoint(lon, lat);
    }

    /**
     * 添加历史路线
     * @param item 历史路线
     * @return 添加结果
     */
    public int addHistoryRoute(final HistoryRouteItemBean item) {
        if (isLogin()) {
            return mUserTrackAdapter.addHistoryRoute(item);
        } else {
            addHistoryRouteToDB(item);
        }
        return 0;
    }

    /**
     * 添加历史路线到数据库
     * @param item 历史路线
     */
    private void addHistoryRouteToDB(final HistoryRouteItemBean item) {
        if (item == null) {
            return;
        }
        final History history = new History();
        history.setMType(AutoMapConstant.SearchKeywordRecordKey.SEARCH_NAVI_RECORD_KEY);
        history.setMPoiId(item.getToPoi().getPoiId());
        history.setMStartPoint(item.getFromPoi().getPoiLoc().toString());
        history.setMEndPoint(item.getToPoi().getPoiLoc().toString());
        history.setMStartPoiName(item.getFromPoi().getName());
        history.setMEndPoiName(item.getToPoi().getName());
        history.setMIsCompleted(item.getIsCompleted());
        history.setMUpdateTime(item.getTime());
        mHistoryManager.insertOrReplace(history);
    }

    /**
     * 删除历史路线
     * @param bean 历史路线名称
     * @return 删除结果
     */
    public int delHistoryRoute(final HistoryRouteItemBean bean) {
        if (isLogin()) {
            return mUserTrackAdapter.delHistoryRoute(bean);
        } else {
            return delHistoryRouteFromDB(bean);
        }
   }

    /**
     * 从数据库删除历史路线
     * @param bean 历史路线
     * @return 删除结果
     */
    private int delHistoryRouteFromDB(final HistoryRouteItemBean bean) {
        if (bean == null) {
            return -1;
        }
        final List<History> history = mHistoryManager.getByStartEndPoiAndTime(
                bean.getFromPoi().getName(), bean.getToPoi().getName(), bean.getTime());
        if (history != null) {
            mHistoryManager.deleteByStartEndPoiAndTime(
                    bean.getFromPoi().getName(), bean.getToPoi().getName(), bean.getTime());
        }
        return 0;
    }

    /**
     * 删除历史路线
     * @return 删除结果
     */
    public int clearHistoryRoute() {
        if (isLogin()) {
            return mUserTrackAdapter.clearHistoryRoute();
        } else {
            mHistoryManager.deleteValueByKey(AutoMapConstant.SearchKeywordRecordKey.SEARCH_NAVI_RECORD_KEY);
        }
        return 0;
    }

    /**
     * 从sdk获取行程数据列表保存到本地
     */
    public void getDrivingRecordData() {
        mUserTrackAdapter.getDrivingRecordData();
    }

    /**
     * 获取行为数据id列表
     * @return 行为数据id列表
     */
    public int[] getBehaviorDataIds() {
        return mUserTrackAdapter.getBehaviorDataIds();
    }

    /**
     * 获取行程数据列表（默认导航历史）
     * @return 行程数据列表
     */
    public ArrayList<DrivingRecordDataBean> getDrivingRecordDataList() {
        return mUserTrackAdapter.getDrivingRecordDataList();
    }

    /**
     * 获取巡航历史-行程数据列表（巡航历史）
     * @return 行程数据列表
     */
    public ArrayList<DrivingRecordDataBean> getDrivingRecordCruiseDataList() {
        return mUserTrackAdapter.getDrivingRecordCruiseDataList();
    }

    /**
     * 从sdk获取当前用户行程数据列表（默认导航历史）
     * @return 行程数据列表
     */
    public ArrayList<DrivingRecordDataBean> getDrivingRecordDataFromSdk() {
        return mUserTrackAdapter.getDrivingRecordDataFromSdk();
    }

    /**
     * 获取所有行程总里程
     * @return 所有行程总里程
     */
    public int getTotalDuration() {
        return mUserTrackAdapter.getTotalDuration();
    }

    /**
     * 获取所有行程总时长
     * @return 所有行程总时长
     */
    public int getTotalDistance() {
        return mUserTrackAdapter.getTotalDistance();
    }

    /**
     * 设置行程信息
     * 写到同步库里面，点击立即同步同步库进行同步，先进行数据同步，再进行轨迹同步
     * @param trailDriveDataId 行程ID
     * @param data 行程信息
     * @return 设置结果
     */
    public int setBehaviorData(final String trailDriveDataId, final String data) {
        Logger.i(TAG, "setBehaviorData  trailDriveDataId: " + trailDriveDataId + " data: " + data);
        final int result = mUserTrackAdapter.setBehaviorData(trailDriveDataId, data);
        Logger.i(TAG, "setBehaviorData result: " + result);
        return result;
    }

    /**
     * 根据ID删除行程信息
     * @param id 行程ID
     * @return 删除结果
     */
    public int delBehaviorData(final String id) {
        return mUserTrackAdapter.delBehaviorData(id);
    }

    /**
     * 获取置顶id文件路径，用户同步数据
     * @param id 行程ID
     * @return 文件路径
     */
    public String getFilePath(final String id) {
        return mUserTrackAdapter.getFilePath(id);
    }

    /**
     * 启动Gps打点，并生成轨迹文件
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @param un32MsecRate 打点频率（单位毫秒，值不低于500），如传入5000表示，每5000毫秒打一个GPS点
     * @return 结果值
     */
    public int startGpsTrack(final String psSavePath, final String psFileName, final long un32MsecRate) {
        // 返回 0 调用成功
        return mUserTrackAdapter.startGpsTrack(psSavePath, psFileName, un32MsecRate);
    }

    /**
     * 关闭Gps打点，并生成轨迹文件
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @return 结果值
     */
    public int closeGpsTrack(final String psSavePath, final String psFileName) {
        return mUserTrackAdapter.closeGpsTrack(psSavePath, psFileName);
    }

    /**
     * 获取指定轨迹文件的深度信息，通过异步回调返回。
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @return 结果值
     */
    public int obtainGpsTrackDepInfo(final String psSavePath, final String psFileName) {
        Logger.i(TAG, "obtainGpsTrackDepInfo: psSavePath = " + psSavePath + ", psFileName = " + psFileName);
        return mUserTrackAdapter.obtainGpsTrackDepInfo(psSavePath, psFileName);
    }

    @Override
    public void notify(final int eventType, final int exCode) {
        for (UserTrackCallBack observer : mCallBacks.values()) {
            observer.notify(eventType, exCode);
        }
    }

    @Override
    public void onStartGpsTrack(final int n32SuccessTag, final String psSavePath, final String psFileName) {
        // 可以调用 setBehaviorData 接口 保存未完成的行程数据(没有终点信息)
        for (UserTrackCallBack observer : mCallBacks.values()) {
            observer.onStartGpsTrack(n32SuccessTag, psSavePath, psFileName);
        }
    }

    /**
     * 从数据库获取行程数据
     * @return 获取数据库中的行程数据
     */
    public ArrayList<DrivingRecordDataBean> getDrivingRecordDataFromDB() {
        final ArrayList<DrivingRecordDataBean> list = new ArrayList<>();
        final List<History> historyList = mHistoryManager.getValueByType(2);
        if (historyList != null && !historyList.isEmpty()) {
            for (History history : historyList) {
                final DrivingRecordDataBean dataBean = new DrivingRecordDataBean();
                dataBean.setId(history.getMPoiId());// 数据ID
                dataBean.setStartPoiName(history.getMStartPoiName()); // 设置起点
                dataBean.setEndPoiName(history.getMEndPoiName()); // 设置终点
                dataBean.setStartLocation(history.getMStartPoint()); // 数据ID
                dataBean.setEndLocation(history.getMEndPoint()); // 数据ID
                dataBean.setRunDistance(history.getMRunDistance()); // 该行程行驶距离
                dataBean.setStartTime(history.getMStartTime());
                dataBean.setRideRunType(history.getMRideRunType()); // 行程类型（导航/巡航）
                dataBean.setTimeInterval(history.getMTimeInterval()); // 驾驶时长
                dataBean.setAverageSpeed(history.getMAverageSpeed()); // 平均速度
                dataBean.setMaxSpeed(history.getMMaxSpeed()); // 最快速度
                dataBean.setTrackFileName(history.getMTrackFileName());
                dataBean.setFilePath(history.getMFilePath());
                list.add(dataBean);
            }
            return list;
        }
        return null;
    }

    private static final int MINIMUM_REQUIRED_DISTANCE_NAVI = 500;
    private static final int MINIMUM_REQUIRED_DISTANCE_CRUISE = 2000;
    @Override
    public void onCloseGpsTrack(final int n32SuccessTag, final String psSavePath, final String psFileName, final GpsTrackDepthBean depInfo) {

        if (depInfo == null || depInfo.getTrackPoints().isEmpty()) {
            Logger.i(TAG, "onCloseGpsTrack: 轨迹信息为空，不保存轨迹信息");
            return;
        }

        Logger.i(TAG, "onCloseGpsTrack: 轨迹信息：" + GsonUtils.toJson(depInfo));

        this.mDepInfo = depInfo;
        this.mPsFileName = psFileName;

        // 获取类型
        final String rideRunType = psFileName.split(mSPLIT)[1];
        if ("1".equals(rideRunType) && depInfo.getDistance() < MINIMUM_REQUIRED_DISTANCE_NAVI) {
            Logger.i(TAG, "onCloseGpsTrack: 导航里程小于500m，不保存轨迹信息");
            return;
        } else if ("0".equals(rideRunType) && depInfo.getDistance() < MINIMUM_REQUIRED_DISTANCE_CRUISE) {
            Logger.i(TAG, "onCloseGpsTrack: 巡航里程小于2000m，不保存轨迹信息");
            return;
        }
        if (n32SuccessTag >= 0) {
            Logger.d(TAG, "onCloseGpsTrack: 保存轨迹信息");
            if (AccountPackage.getInstance().isLogin()) {
                saveGpsTrackToDB(psSavePath, psFileName, depInfo);
                saveGpsTrack(psSavePath, psFileName, depInfo);
            } else {
                saveGpsTrackToDB(psSavePath, psFileName, depInfo);
            }
        }
        for (UserTrackCallBack observer : mCallBacks.values()) {
            observer.onCloseGpsTrack(n32SuccessTag, psSavePath, psFileName, depInfo);
        }
    }

    @Override
    public void onGpsTrackDepInfo(final int n32SuccessTag, final String psSavePath, final String psFileName, final GpsTrackDepthBean depInfo) {
        for (UserTrackCallBack observer : mCallBacks.values()) {
            observer.onGpsTrackDepInfo(n32SuccessTag, psSavePath, psFileName, depInfo);
        }
    }

    /**
     * 提取重复的地理坐标转换逻辑
     * @param longitude 经度
     * @param latitude 纬度
     * @return 转换后的地理坐标
     */
    private JSONObject createGeoJson(final double longitude, final double latitude) throws JSONException {
        final JSONObject loc = new JSONObject();
        loc.put(KEY_X, String.valueOf(longitude));
        loc.put(KEY_Y, String.valueOf(latitude));
        return loc;
    }

    /**
     * 提取重复的时间格式化逻辑
     * @param timestamp 时间戳
     * @return 格式化后的时间字符串
     */
    private String formatTimestamp(final long timestamp) {
        return new SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.getDefault())
                .format(timestamp);
    }

    /**
     * 保存轨迹信息云端
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @param depInfo 轨迹深度信息
     */
    public void saveGpsTrack(final String psSavePath, final String psFileName, final GpsTrackDepthBean depInfo) {
        try {
            if (depInfo == null || depInfo.getTrackPoints().isEmpty()) {
                Logger.e(TAG, "轨迹数据为空");
                return;
            }


            // 使用辅助方法简化代码
            buildCommonJsonFields(psFileName, depInfo);
            buildLocationSpecificFields(depInfo);

            Logger.d(TAG, "云端轨迹数据构建完成");
        } catch (JSONException e) {
            Logger.e(TAG, WRONG, e.getMessage());
            // 添加异常恢复逻辑
            resetTrackData();
        }
    }


    /**
     * 重置值
     */
    private void resetTrackData() {
        this.mDepInfo = null;
        this.mPsFileName = null;
        this.mReverseType = -1;
        mJson = new JSONObject(); // 需要将mJson改为非final
    }

    /**
     * 构建公共的JSON字段
     * @param psFileName GPS轨迹文件名
     * @param depInfo 轨迹深度信息
     * @throws JSONException JSON异常
     */
    private void buildCommonJsonFields(final String psFileName, final GpsTrackDepthBean depInfo) throws JSONException {
        final String rideRunType = psFileName.split(mSPLIT)[1];
        mJson.put("id", psFileName)
                .put("type", 403)
                .put("rideRunType", rideRunType)
                .put("timeInterval", depInfo.getDuration())
                .put("runDistance", depInfo.getDistance())
                .put("maxSpeed", (int) depInfo.getTrackPoints().get(depInfo.getFastestIndex()).getF32Speed() + "");
    }

    /**
     * 构建位置特定的JSON字段
     * @param depInfo 轨迹深度信息
     * @throws JSONException JSON异常
     */
    private void buildLocationSpecificFields(final GpsTrackDepthBean depInfo) throws JSONException {
        // 添加空检查和边界校验
        if (depInfo == null || depInfo.getTrackPoints().isEmpty()) {
            throw new IllegalArgumentException("Invalid track points data");
        }
        final ArrayList<GpsTrackPointBean> points = depInfo.getTrackPoints();
        mJson.put("startTime", formatTimestamp(points.get(0).getN64TickTime()))
                .put("endTime", formatTimestamp(points.get(points.size() - 1).getN64TickTime()))
                .put("trackFileName", mPsFileName)
                .put("startLocation", createGeoJson(
                        points.get(0).getF64Longitude(),
                        points.get(0).getF64Latitude()
                ).toString())
                .put("endLocation", createGeoJson(
                        points.get(points.size() - 1).getF64Longitude(),
                        points.get(points.size() - 1).getF64Latitude()
                ).toString());
    }



    /**
     * 保存轨迹信息本地
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @param depInfo 轨迹深度信息
     */
    private void saveGpsTrackToDB(final String psSavePath, final String psFileName, final GpsTrackDepthBean depInfo) {
        try {

            // 添加参数校验
            if (TextUtils.isEmpty(psFileName) || depInfo == null || depInfo.getTrackPoints().isEmpty()) {
                Logger.e(TAG, "Invalid parameters in saveGpsTrackToDB");
                return;
            }

            // 添加分割结果校验
            final String[] parts = psFileName.split(mSPLIT);
            if (parts.length < 2) {
                Logger.e(TAG, "Invalid filename format: " + psFileName);
                return;
            }

            final ArrayList<GpsTrackPointBean> points = depInfo.getTrackPoints();

            // 添加轨迹点索引保护
            final int lastIndex = points.size() - 1;
            if (lastIndex < 0) {
                Logger.d("index invalid, no track points available");
                return;
            }

            final String rideRunType = parts[1];
            mHistory.setMPoiId(psFileName);
            final JSONObject startLoc = new JSONObject();
            startLoc.put(KEY_X, points.get(0).getF64Longitude() + "");
            startLoc.put(KEY_Y, points.get(0).getF64Latitude() + "");
            mHistory.setMStartPoint(startLoc.toString());
            final JSONObject endLoc = new JSONObject();
            endLoc.put(KEY_X, points.get(points.size() - 1).getF64Longitude() + "");
            endLoc.put(KEY_Y, points.get((points.size() - 1)).getF64Latitude() + "");
            mHistory.setMEndPoint(endLoc.toString());
            mHistory.setMRunDistance((int)depInfo.getDistance());// 该行程行驶距离
            final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.getDefault());
            final String startTime = dateFormat.format(points.get(0).getN64TickTime());
            mHistory.setMStartTime(startTime); // 该行程开始时间
            mHistory.setMRideRunType(Integer.parseInt(rideRunType)); // 行程类型（导航/巡航）
            mHistory.setMTimeInterval((int)depInfo.getDuration()); // 驾驶时长
            mHistory.setMAverageSpeed((int)depInfo.getAverageSpeed()); // 平均速度
            mHistory.setMMaxSpeed((int) points.get(depInfo.getFastestIndex()).getF32Speed()); // 最快速度
            mHistory.setMTrackFileName(psFileName);
            mHistory.setMFilePath(psSavePath);
            mHistory.setMType(AutoMapConstant.SearchKeywordRecordKey.DRIVING_HISTORY_RECORD_KEY); // 该条记录类型
            geoSearch(REVERSE_START, new GeoPoint(points.get(0).getF64Longitude(),
                    points.get(0).getF64Latitude()));
        } catch (JSONException e) {
            Logger.e(TAG, WRONG, e.getMessage());
        }
    }

    /**
     * 逆地理
     * @param reverseType 反地理编码类型
     * @param geoPoint 坐标点
     */
    public void geoSearch(final int reverseType, final GeoPoint geoPoint) {
        Logger.i(TAG, "geoSearch: " + reverseType + " " + geoPoint.getLon() + " " + geoPoint.getLat());
        this.mReverseType = reverseType;
        SearchPackage.getInstance().geoSearch(geoPoint);
    }

    @Override
    public void onSearchResult(final int taskId, final int errorCode, final String message, final SearchResultEntity searchResultEntity) {

        if (mDepInfo == null || mDepInfo.getTrackPoints().isEmpty() || TextUtils.isEmpty(mPsFileName) || mReverseType == -1) {
            Logger.d(TAG, "轨迹信息 或 文件名称 或 反地理编码类型为空，不进行逆地理编码");
            return;
        }

        if (searchResultEntity == null || searchResultEntity.getPoiList() == null || searchResultEntity.getPoiList().isEmpty()) {
            Logger.d(TAG, "逆地理编码结果为空");
            return;
        }

        try {
            if (mReverseType == REVERSE_START){
                if (searchResultEntity.getPoiList() != null && !searchResultEntity.getPoiList().isEmpty()) {
                    mJson.put("startPoiName", searchResultEntity.getPoiList().get(0).getName());
                    mHistory.setMStartPoiName(searchResultEntity.getPoiList().get(0).getName());
                } else {
                    mJson.put("startPoiName", DEFAULT_NAME);
                    mHistory.setMStartPoiName(DEFAULT_NAME);
                }
                geoSearch(REVERSE_END, new GeoPoint(mDepInfo.getTrackPoints().get(mDepInfo.getTrackPoints().size() - 1).getF64Longitude(),
                        mDepInfo.getTrackPoints().get(mDepInfo.getTrackPoints().size() - 1).getF64Latitude()));
            } else if (mReverseType == REVERSE_END){
                if (searchResultEntity.getPoiList() != null && !searchResultEntity.getPoiList().isEmpty()) {
                    mJson.put("endPoiName", searchResultEntity.getPoiList().get(0).getName());
                    mHistory.setMEndPoiName(searchResultEntity.getPoiList().get(0).getName());
                } else {
                    mJson.put("endPoiName", DEFAULT_NAME);
                    mHistory.setMEndPoiName(DEFAULT_NAME);
                }
                geoSearch(REVERSE_FASTEST, new GeoPoint(mDepInfo.getTrackPoints().get(mDepInfo.getFastestIndex()).getF64Longitude(),
                        mDepInfo.getTrackPoints().get(mDepInfo.getFastestIndex()).getF64Latitude()));
                mHistoryManager.insertOrReplace(mHistory);
            } else if (mReverseType == REVERSE_FASTEST){
                if (searchResultEntity.getPoiList() != null && !searchResultEntity.getPoiList().isEmpty()) {
                    mJson.put("maxSpeedPoiName", searchResultEntity.getPoiList().get(0).getName());
                } else {
                    mJson.put("maxSpeedPoiName", DEFAULT_NAME);
                }
                if (AccountPackage.getInstance().isLogin()) {
                    setBehaviorData(mPsFileName, mJson.toString());
                }
                resetTrackData();
                Logger.d(TAG, "保存轨迹数据成功");
            }
        } catch (JSONException e) {
            Logger.e(TAG, WRONG, e.getMessage());
        }
    }

    public static UserTrackPackage getInstance() {
        return Helper.EP;
    }

    private static final class Helper {
        private static final UserTrackPackage EP = new UserTrackPackage();
    }

}
