package com.sgm.navi.burypoint;

public class DataTrackUtils {

    private static final String TAG = DataTrackUtils.class.getSimpleName();
    private static final String IMPLICIT_TAR_SCREEN = "IMPLICIT_TAR_SCREEN";
    private static final String IMPLICIT_TAR_ACTIVITY = "IMPLICIT_TAR_ACTIVITY";
    private static final String EMPTY_STRING = "";
    public static final String JUMP_APP_INFO = "jump_app_info";
    public static final String TARGET_APP = "target_app";
    public static final String TARGET_SCREEN = "target_screen";
    public static final String TARGET_ACTIVITY = "target_activity";

    private DataTrackUtils() {
    }

    private static class DataTrackUtilsHolder {
        private static final DataTrackUtils INSTANCE = new DataTrackUtils();
    }

    public static DataTrackUtils getInstance() {
        return DataTrackUtils.DataTrackUtilsHolder.INSTANCE;
    }

    /**
     * 页面进入时埋点
     *
     * @param curScreen   当前页面的{@link ScreenName}
     * @param curActivity 当前页面的activity名字
     */
    public void pageEnter(String curScreen, String curActivity) {
        BuryManager.getInstance().getPatacDataTrackManager().trackEnterPage(
                BuryManager.getInstance().getSid(),
                BuryManager.getInstance().getSvid(),
                BuryManager.getInstance().getAppId(),
                BuryManager.getInstance().getAppVersion(),
                curScreen,
                curActivity,
                null
        );
    }

    /**
     * 页面退出时埋点
     *
     * @param curScreen   当前页面的{@link ScreenName}
     * @param curActivity 当前页面的activity名字
     */
    public void pageExit(String curScreen, String curActivity) {
        BuryManager.getInstance().getPatacDataTrackManager().trackExitPage(
                BuryManager.getInstance().getSid(),
                BuryManager.getInstance().getSvid(),
                BuryManager.getInstance().getAppId(),
                BuryManager.getInstance().getAppVersion(),
                curScreen,
                curActivity,
                null
        );
    }

    /**
     * app内部页面跳转时进行埋点
     *
     * @param curScreen   当前页面的{@link ScreenName}
     * @param curActivity 当前页面的activity名字
     * @param tarScreen   目标页面的{@link ScreenName}
     * @param tarActivity 目标页面的activity名字
     */
    public void trackJumpPage(String curScreen, String curActivity, String tarScreen, String tarActivity) {
        BuryManager.getInstance().getPatacDataTrackManager().trackJumpPage(
                BuryManager.getInstance().getSid(),
                BuryManager.getInstance().getSvid(),
                BuryManager.getInstance().getAppId(),
                BuryManager.getInstance().getAppVersion(),
                curScreen,
                curActivity,
                tarScreen,
                tarActivity,
                null
        );
    }

    /**
     * APP内部页面跳转时进行埋点--隐式方式启动页面时用此方法
     *
     * @param curScreen   当前页面的{@link ScreenName}
     * @param curActivity 当前页面的activity名字
     */
    public void trackJumpPage(String curScreen, String curActivity) {
        trackJumpPage(curScreen, curActivity, IMPLICIT_TAR_SCREEN, IMPLICIT_TAR_ACTIVITY);
    }

    /**
     * 跨APP页面跳转时进行埋点
     *
     * @param curScreen   当前页面的{@link ScreenName}
     * @param curActivity 当前页面的activity名字
     * @param tarApp      目前APP包名
     * @param tarScreen   目标页面的{@link ScreenName}
     * @param tarActivity 目标页面的activity名字
     */
    public void trackJumpApp(String curScreen, String curActivity, String tarApp, String tarScreen, String tarActivity) {
        BuryManager.getInstance().getPatacDataTrackManager().trackJumpApp(
                BuryManager.getInstance().getSid(),
                BuryManager.getInstance().getSvid(),
                BuryManager.getInstance().getAppId(),
                BuryManager.getInstance().getAppVersion(),
                curScreen,
                curActivity,
                tarApp,
                tarScreen,
                tarActivity,
                null
        );
    }

    /**
     * 跨APP页面跳转时进行埋点--隐式方式启动页面时用此方法
     *
     * @param curScreen   当前页面的{@link ScreenName}
     * @param curActivity 当前页面的activity名字
     * @param tarApp      目前APP包名
     */
    public void trackJumpApp(String curScreen, String curActivity, String tarApp) {
        trackJumpApp(curScreen, curActivity, tarApp, IMPLICIT_TAR_SCREEN, IMPLICIT_TAR_ACTIVITY);
    }

    /**
     * 具体业务埋点(非通用埋点场景调用此接口)
     *
     * @param sid         埋点的数据模板ID
     * @param svid        埋点的数据模板版本ID
     * @param appid       埋点的 appid
     * @param eventName   埋点事件名称
     * @param curScreen   当前页面的{@link ScreenName}(埋点场景没有对应的页面时传空字符串)
     * @param curActivity 当前页面的activity名字(埋点场景没有对应的页面时传空字符串)
     * @param properties  具体埋点数据，一个埋点事件根据需求需要一次上传完所有事件属性，其中key必须传字段编号：Gxxx，value传真实的值
     */
    public void track(String sid, String svid, String appid, String eventName, String curScreen, String curActivity, String properties) {
        BuryManager.getInstance().getPatacDataTrackManager().track(
                eventName,
                sid,
                svid,
                appid,
                BuryManager.getInstance().getAppVersion(),
                curScreen,
                curActivity,
                properties
        );
    }

    /**
     * 具体业务埋点，此接口如果是和BaseApplication中传的sid，svid，appid不一致，有特殊的埋点模板，那就调用此接口
     * (非通用埋点场景且没有相关页面时调用此接口)
     *
     * @param sid        埋点的数据模板ID
     * @param svid       埋点的数据模板版本ID
     * @param appid      埋点的 appid
     * @param eventName  埋点事件名称
     * @param properties 具体埋点数据，一个埋点事件根据需求需要一次上传完所有事件属性，其中key必须传字段编号：Gxxx，value传真实的值
     */
    public void track(String sid, String svid, String appid, String eventName, String properties) {
        track(sid, svid, appid, eventName, EMPTY_STRING, EMPTY_STRING, properties);
    }

    /**
     * 具体业务埋点，此接口如果确定埋点数据模板是和BaseApplication中传的sid，svid，appid一致，就调用此接口，
     * 此接口中将BaseApplication中的sid，svid，appid都封装好了，如果有特殊的数据模板需求，请勿调用此接口
     * (非通用埋点场景且没有相关页面时调用此接口)
     *
     * @param eventName  埋点事件名称
     * @param properties 具体埋点数据，一个埋点事件根据需求可能对应多个埋点事件属性，多个埋点事件属性需要封装好上传到此字段中，
     *                   包含key和value，其中key必须传字段编号：Gxxx（请注意如果传为字段名称会造成后台脏数据，导致无法解析），value传真实的值
     */
    public void track(String eventName, String properties) {
        BuryManager.getInstance().getPatacDataTrackManager().track(
                eventName,
                BuryManager.getInstance().getSid(),
                BuryManager.getInstance().getSvid(),
                BuryManager.getInstance().getAppId(),
                BuryManager.getInstance().getAppVersion(),
                EMPTY_STRING,
                EMPTY_STRING,
                properties
        );
    }

}
