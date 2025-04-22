package com.fy.navi.scene.ui.navi.manager;

import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.MapDefaultFinalTag;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * @author lww
 * @date 2025/3/20
 */
public class NaviSceneManager implements INaviSceneEvent {
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_TAG;
    /*** 所有的Scene **/
    private final ConcurrentMap<NaviSceneId, NaviSceneBase> sceneViewList;
    /*** 正在显示的Scene **/
    private final CopyOnWriteArrayList<NaviSceneBase> showSceneList;
    /*** 被隐藏的Scene **/
    private final CopyOnWriteArrayList<NaviSceneBase> hideSceneList;
    private boolean mIsCanAddScene;

    private final List<NaviSceneId> hideByMoreSetup = new ArrayList<>(Arrays.asList(
            NaviSceneId.NAVI_SCENE_PARK_LIST, NaviSceneId.NAVI_SUSPEND_CARD, NaviSceneId.NAVI_CHARGE_TIP,
            NaviSceneId.NAVI_SCENE_SERVICE_AREA, NaviSceneId.NAVI_SCENE_2D_CROSS));

    private NaviSceneManager() {
        sceneViewList = new ConcurrentHashMap<>();
        showSceneList = new CopyOnWriteArrayList<>();
        hideSceneList = new CopyOnWriteArrayList<>();
    }

    public static NaviSceneManager getInstance() {
        return Helper.sm;
    }

    private void onHideScene(NaviSceneId cardId) {
        NaviSceneBase sceneBase = getSceneById(cardId);
        if (ConvertUtils.isEmpty(sceneBase)) {
            Logger.i(TAG, "sceneBase is null");
            return;
        }
        if (NaviSceneBase.SCENE_STATE_HIDE == sceneBase.getSceneState() || NaviSceneBase.SCENE_STATE_CLOSE == sceneBase.getSceneState()) {
            return;
        }
        Logger.i(TAG, "onHideScene Name -> " + sceneBase.getSceneName());
        hideScene(sceneBase);
    }

    private void onShowScene(NaviSceneId cardId) {
        ThreadManager.getInstance().postUi(() -> {
            NaviSceneBase newSceneBase = getSceneById(cardId);
            if (!ConvertUtils.isEmpty(newSceneBase)) { // 先判断集合中是否已经包含该场景
                if (ConvertUtils.isEmpty(showSceneList)) { // 如果showSceneList为空则直接展示
                    Logger.i(TAG, "没有任何卡片在显示，直接展示新卡片", "newSceneBase -> " + newSceneBase.getSceneName());
                    showScene(newSceneBase);
                } else {
                    if (NaviSceneBase.SCENE_STATE_SHOW == newSceneBase.getSceneState()) { // 正在展示中不进行任何操作
                        Logger.i(TAG, "current " + newSceneBase.getSceneName() + " is show");
                    } else { // 没在展示，则根据碰撞规则展示Scene
                        ArrayList<NaviSceneBase> temporaryList = new ArrayList<>(showSceneList);
                        int showSceneListSize = temporaryList.size(); // 有人知道为什么要用size，而不是直接用showSceneList.size吗， 动脑筋猜一猜
                        Logger.d(TAG, "当前展示的Scene集合长度:" + showSceneListSize + " newName:" + newSceneBase.getSceneName() + " newState:" + newSceneBase.getSceneState() + " newId:" + cardId.ordinal());
                        StringBuilder logShowAndShow = new StringBuilder();
                        StringBuilder logShowAndClose = new StringBuilder();
                        StringBuilder logShowAndHide = new StringBuilder();
                        StringBuilder logHideAndShow = new StringBuilder();
                        StringBuilder logCloseAndShow = new StringBuilder();
                        boolean isShowAndShow = false;
                        boolean isShowAndClose = false;
                        boolean isShowAndHide = false;
                        boolean isHideAndShow = false;
                        boolean isCloseAndShow = false;
                        for (int i = 0; i < showSceneListSize; i++) {// 遍历所有显示的Scene，在根据定制的规则来判定正在显示的Scene是否需要隐藏或关闭
                            if (i >= temporaryList.size()) {
                                Logger.e(TAG, "ArrayIndexOutOfBoundsException", "i:" + i, "size:" + temporaryList.size());
                                break;
                            }
                            NaviSceneBase oldSceneView = temporaryList.get(i);
                            int sceneRule = getSceneRule(oldSceneView.getSceneId(), cardId);
                            switch (sceneRule) {
                                case NaviSceneRule.SCENE_SHOW_AND_SHOW -> {
                                    //Logger.i(TAG, "双卡同时显示", "oldSceneView:" + oldSceneView.getSceneName() + " oldId:" + oldSceneView.getSceneId().ordinal());
                                    if (!isShowAndShow) {
                                        isShowAndShow = true;
                                        logShowAndShow.append("双卡同时显示:");
                                    }
                                    logShowAndShow.append(oldSceneView.getSceneId().ordinal()).append(":").append(oldSceneView.getSceneName()).append(" ");
                                    showScene(newSceneBase);
                                }
                                case NaviSceneRule.SCENE_SHOW_AND_CLOSE -> {
                                    //Logger.i(TAG, "旧卡继续显示，新卡不展示", "oldSceneView:" + oldSceneView.getSceneName() + " oldId:" + oldSceneView.getSceneId().ordinal());
                                    if (!isShowAndClose) {
                                        isShowAndClose = true;
                                        logShowAndClose.append("旧卡继续显示，新卡不展示:");
                                    }
                                    logShowAndClose.append(oldSceneView.getSceneId().ordinal()).append(":").append(oldSceneView.getSceneName()).append(" ");
                                    closeScene(newSceneBase);
                                    return; // 有人知道为什么要用return吗
                                }
                                case NaviSceneRule.SCENE_SHOW_AND_HIDE -> {
                                    //Logger.i(TAG, "旧卡继续显示，新卡暂时不显示，待旧卡显示结束显卡再显示", "oldSceneView:" + oldSceneView.getSceneName() + " oldId:" + oldSceneView.getSceneId().ordinal());
                                    if (!isShowAndHide) {
                                        isShowAndHide = true;
                                        logShowAndHide.append("旧卡继续显示，新卡暂时不显示，待旧卡显示结束显卡再显示:");
                                    }
                                    logShowAndHide.append(oldSceneView.getSceneId().ordinal()).append(":").append(oldSceneView.getSceneName()).append(" ");
                                    // TODO: 2025/3/22 等待旧卡显示完毕 打开新卡
                                    hideScene(newSceneBase);
                                    return; // 有人知道为什么要用return吗
                                }
                                case NaviSceneRule.SCENE_HIDE_AND_SHOW -> {
                                    //Logger.i(TAG, "旧卡隐藏，开始显示新卡，新卡显示结束继续显示旧卡", "oldSceneView:" + oldSceneView.getSceneName() + " oldId:" + oldSceneView.getSceneId().ordinal());
                                    if (!isHideAndShow) {
                                        isHideAndShow = true;
                                        logHideAndShow.append("旧卡继续显示，新卡暂时不显示，待旧卡显示结束显卡再显示:");
                                    }
                                    logHideAndShow.append(oldSceneView.getSceneId().ordinal()).append(":").append(oldSceneView.getSceneName()).append(" ");
                                    hideScene(oldSceneView);
                                    showScene(newSceneBase);
                                }
                                case NaviSceneRule.SCENE_CLOSE_AND_SHOW -> {
                                    //Logger.i(TAG, "旧卡关闭，开始显示新卡", "oldSceneView:" + oldSceneView.getSceneName() + " oldId:" + oldSceneView.getSceneId().ordinal();
                                    if (!isCloseAndShow) {
                                        isCloseAndShow = true;
                                        logCloseAndShow.append("旧卡关闭，开始显示新卡:");
                                    }
                                    logCloseAndShow.append(oldSceneView.getSceneId().ordinal()).append(":").append(oldSceneView.getSceneName()).append(" ");
                                    closeScene(oldSceneView);
                                    showScene(newSceneBase);
                                }
                                default ->
                                        Logger.i(TAG, "不做任何处理", "oldSceneView:" + oldSceneView.getSceneName() + " oldId:" + oldSceneView.getSceneId().ordinal(),
                                                " newName:" + newSceneBase.getSceneName() + " newState:" + newSceneBase.getSceneState() + " newId:" + cardId.ordinal());
                            }
                        }
                        if (isShowAndShow) Logger.i(TAG, logShowAndShow);
                        if (isShowAndClose) Logger.i(TAG, logShowAndClose);
                        if (isShowAndHide) Logger.i(TAG, logShowAndHide);
                        if (isHideAndShow) Logger.i(TAG, logHideAndShow);
                        if (isCloseAndShow) Logger.i(TAG, logCloseAndShow);
                    }
                }
            } else { // 不包含该场景代表这个Scene不受规则控制
                Logger.i(TAG, "不包含此场景");
            }
        });
    }

    private void onCloseScene(NaviSceneId cardId) {
        NaviSceneBase sceneBase = getSceneById(cardId);
        if (ConvertUtils.isEmpty(sceneBase)) {
            return;
        }
        if (NaviSceneBase.SCENE_STATE_CLOSE == sceneBase.getSceneState()) {
            return;
        }
        Logger.i(TAG, "onCloseScene", "Name:" + sceneBase.getSceneName() + " State:" + sceneBase.getSceneState());
        closeScene(sceneBase);
    }

    private void hideScene(@Nullable NaviSceneBase sceneView) {
        if (sceneView == null) {
            Logger.e(TAG, "sceneView==null");
            return;
        }
        if (NaviSceneBase.SCENE_STATE_CLOSE == sceneView.getSceneState()
                || NaviSceneBase.SCENE_STATE_HIDE == sceneView.getSceneState()) {
            Logger.i(TAG, "current sceneView is close 不做处理: " + sceneView.getSceneName());
            return;
        }
        //Logger.i(TAG, "sceneView -> " + sceneView.getSceneName());
        if (!hideSceneList.contains(sceneView)) {
            hideSceneList.add(sceneView);
        } else {
            Logger.i(TAG, "hideSceneList 已存在！：" + sceneView.getSceneId().name());
        }
        sceneView.hide();
        if (ConvertUtils.isContain(showSceneList, sceneView)) {
            ConvertUtils.remove(showSceneList, sceneView);
        }
    }

    private void showScene(@Nullable NaviSceneBase sceneView) {
        if (sceneView == null) {
            Logger.e(TAG, "sceneView==null");
            return;
        }
        //Logger.i(TAG, "showScene -> " + sceneView.getSceneName(), " getSceneState:" + sceneView.getSceneState());
        if (NaviSceneBase.SCENE_STATE_SHOW == sceneView.getSceneState()) return;
        if (!showSceneList.contains(sceneView)) {
            showSceneList.add(sceneView);
        } else {
            Logger.i(TAG, "showSceneList 已存在！：" + sceneView.getSceneId().name());
        }
        sceneView.show();
        if (ConvertUtils.isContain(hideSceneList, sceneView)) {
            ConvertUtils.remove(hideSceneList, sceneView);
        }
    }

    private void closeScene(@Nullable NaviSceneBase sceneView) {
        if (sceneView == null) {
            Logger.e(TAG, "sceneView==null");
            return;
        }
        //Logger.i(TAG, "closeScene", "sceneView -> " + sceneView.getSceneName() + "," + sceneView.getSceneState());
        if (NaviSceneBase.SCENE_STATE_CLOSE == sceneView.getSceneState()) return;
        sceneView.close();
        if (ConvertUtils.isContain(hideSceneList, sceneView)) {
            ConvertUtils.remove(hideSceneList, sceneView);
        }
        if (ConvertUtils.isContain(showSceneList, sceneView)) {
            ConvertUtils.remove(showSceneList, sceneView);
        }
    }

    //初始化要显示的scene
    public void initShowScene(NaviSceneId cardId) {
        if (cardId == null) {
            Logger.e(TAG, "sceneId==null");
            return;
        }
        NaviSceneBase newSceneBase = getSceneById(cardId);
        if (!ConvertUtils.isEmpty(newSceneBase)) {
            Logger.d(TAG, "getSceneName -> " + newSceneBase.getSceneName(), "getSceneState:" + newSceneBase.getSceneState());
            showScene(newSceneBase);
        } else {
            Logger.e(TAG, "newSceneBase==null");
        }
    }

    /**
     * 碰撞规则.
     *
     * @param oldSceneId 正在显示的Scene标识
     * @param newSceneId 新的Scene标识
     * @return 碰撞规则
     */
    public int getSceneRule(NaviSceneId oldSceneId, NaviSceneId newSceneId) {
        return NaviSceneRule.getCollisionType(oldSceneId, newSceneId);
    }

    /**
     * @param id        id
     * @param naviScene 卡片
     */
    public void addNaviScene(final NaviSceneId id, final NaviSceneBase naviScene) {
        Logger.i(TAG, "addNaviScene", "id -> " + id.name(), "naviScene：" + naviScene.getSceneName() +
                " mIsCanAddScene -> " + mIsCanAddScene);
        // 因为launcher巡航等页面会复用navi的scene所以这里只能在navi页面起来后才能添加scene
        if (!mIsCanAddScene) {
            return;
        }
        if (ConvertUtils.isContain(sceneViewList, id)) {
            Logger.i("addNaviScene", "已存在！：" + naviScene.getSceneId().name());
        } else {
            sceneViewList.put(id, naviScene);
        }
    }

    /**
     * @param id id
     * @return 卡片, 返回值有为空的可能，使用方要进行判断
     */
    public NaviSceneBase getSceneById(final NaviSceneId id) {
        return ConvertUtils.containToValue(sceneViewList, id);
    }

    @Override
    public void notifySceneStateChange(SceneStateChangeType type, NaviSceneId cardId) {
        if (type == SceneStateChangeType.SceneCloseState) {//关闭Scene
            onCloseScene(cardId);
        } else if (type == SceneStateChangeType.SceneShowState) {//显示Scene
            onShowScene(cardId);
        } else if (type == SceneStateChangeType.SceneHideState) {//隐藏Scene
            onHideScene(cardId);
        }
    }

    @Override
    public void notifySceneReset(String from) {
        if (!ConvertUtils.isEmpty(hideSceneList)) {
            StringBuilder logBuilder = new StringBuilder();
            ThreadManager.getInstance().postUi(() -> {
                for (NaviSceneBase newScene : hideSceneList) {
                    logBuilder.append(" ").append(newScene.getSceneName()).append(":").append(newScene.getSceneId());
                    onShowScene(newScene.getSceneId());
                }
            });
            Logger.i(TAG, from + logBuilder);
        }
    }

    public void removeHideSceneList(NaviSceneBase sceneBase) {
        Logger.i(TAG, sceneBase.getSceneName());
        hideSceneList.remove(sceneBase);
    }

    public void clearHideSceneList() {
        if (!ConvertUtils.isEmpty(hideSceneList)) {
            ConvertUtils.clear(hideSceneList);
        }
    }

    @Override
    public void destroySceneView() {
        Logger.i(TAG, "destroySceneView");
        mIsCanAddScene = false;
        ConvertUtils.clear(sceneViewList);
        ConvertUtils.clear(showSceneList);
        ConvertUtils.clear(hideSceneList);
    }

    @Override
    public void onCreateSceneView() {
        Logger.i(TAG, "onCreateSceneView");
        mIsCanAddScene = true;
    }

    //控制栏-更多 跟控制栏一个scene，因此该形态单独处理
    public void showControlMoreSetup() {
        Logger.i(TAG, "showControlMoreSetup");
        for (NaviSceneId sceneId : hideByMoreSetup) {
            NaviSceneBase sceneBase = getSceneById(sceneId);
            if (!ConvertUtils.isEmpty(sceneBase)) {
                if (sceneBase.mSceneState == NaviSceneBase.SCENE_STATE_SHOW) {
                    //该场景下tmc未加入碰撞逻辑，暂时单独处理，以后有好的处理方式再优化
                    if (sceneId == NaviSceneId.NAVI_SCENE_2D_CROSS) {
                        notifySceneReset("showControlMoreSetup");
                    }
                    Logger.i(TAG, "showControlMoreSetup hideSceneName -> " + sceneBase.getSceneName());
                    hideScene(sceneBase);
                }
            } else {
                Logger.e(TAG, "sceneBase is null " + sceneId);
            }
        }
    }

    private static final class Helper {
        private static final NaviSceneManager sm = new NaviSceneManager();
    }
}