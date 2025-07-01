package com.sgm.navi.service.adapter.forcast.bls;

import androidx.work.Data;
import androidx.work.ListenableWorker;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.user.forcast.ForcastService;
import com.autonavi.gbl.user.forcast.model.ForcastArrivedData;
import com.autonavi.gbl.user.forcast.model.ForcastArrivedParam;
import com.autonavi.gbl.user.forcast.model.ForcastInitParam;
import com.autonavi.gbl.user.forcast.observer.IForcastServiceObserver;
import com.autonavi.gbl.util.TimeUtil;
import com.autonavi.gbl.util.model.ServiceInitStatus;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.sgm.navi.service.GBLCacheFilePath;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.forcast.IForCastApi;
import com.sgm.navi.service.define.bean.FyOftenArrivedItem;
import com.sgm.navi.service.define.code.CodeManager;

import java.util.ArrayList;

/**
 * @Description TODO
 * @author lww
 * @date 2025/3/25
 */
public class ForCastApiImpl implements IForCastApi, IForcastServiceObserver {
    private static final String TAG = MapDefaultFinalTag.FOR_CAST_SERVICE_TAG;
    private ForcastService mForcastService;

    public ForCastApiImpl() {
    }

    @Override
    public ListenableWorker.Result initForCastService() {
        int result = initService();
        if (ConvertUtils.equals(0, result)) return ListenableWorker.Result.success();
        Data data = new Data.Builder()
                .putInt("errorCode", 10001)
                .putString("errorMsg", CodeManager.getInstance().getForCastMsg(10001))
                .build();
        return ListenableWorker.Result.failure(data);
    }

    @Override
    public void registerCallback() {

    }

    @Override
    public void unRegisterCallback() {

    }

    @Override
    public void destroyService() {
        mForcastService.removeObserver(this);
        mForcastService.unInit();
    }

    @Override
    public void onlineForCastArrive() {
        checkForCastService();
        //本地预测类常去地点
//        int type = ArrivedType.ForcastLocal;
        //获取常去地点列表
//        ArrayList<OftenArrivedItem> OftenArrivedList = mForcastService.getArrivedDataList(type);

        // 获取在线预测常去地点
        ForcastArrivedParam param = new ForcastArrivedParam();
        param.nLevel = 20; // 图面比例尺级别
        param.adCode = "350200"; // 所在城市对应 adcode
        param.userLoc.lon = 118.185962;
        param.userLoc.lat = 24.489438;
        param.userId = ""; // 登录用户UID
        mForcastService.getOnlineForcastArrivedData(param);
    }

    @Override
    public ArrayList<FyOftenArrivedItem> getArrivedDataList(int type) {
        return null;
    }

    @Override
    public void addArrivedDataList(int type, FyOftenArrivedItem arrivedItem) {

    }

    @Override
    public void delLocalArrivedData(int type, String name) {

    }

    @Override
    public ArrayList<FyOftenArrivedItem> getFrequentItemList() {
        return null;
    }

    @Override
    public void setFrequentItemList(ArrayList<FyOftenArrivedItem> data) {

    }


    @Override
    public void onInit(int result) {
        Logger.i(TAG, "init ForCastService result：" + result);
    }

    @Override
    public void onSetLoginInfo(int result) {
        Logger.i(TAG, "账号登录结果：" + result);
    }

    @Override
    public void onForcastArrivedData(ForcastArrivedData data) {
        Logger.i(TAG, "预测结果：" + data);
    }

    private void checkForCastService() {
        if(ServiceInitStatus.ServiceInitDone != mForcastService.isInit()){
            initService();
        }
    }

    private int initService() {
        if(null == mForcastService)
            mForcastService = (ForcastService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.ForcastSingleServiceID);
        ForcastInitParam forcastInitParam = new ForcastInitParam();
        forcastInitParam.stCurTime = TimeUtil.getLocalTime2(); // 当前时间 com.autonavi.gbl.util.model.DateTime
        forcastInitParam.dbPath = GBLCacheFilePath.BEHAVIOR_WSTR_DB_FILE_DIR; // 预测数据库文件保存目录路径
        forcastInitParam.nMaxEnergyMileage = 50; // 能源消耗保存最大公里数单位(KM)
        forcastInitParam.nTopArrivedMaxCnt = 8; // 常去地点列表最大个数, 也决定了获取常去地点接口返回的最大数据量
        mForcastService.addObserver(this);
        return mForcastService.init(forcastInitParam);
    }
}
