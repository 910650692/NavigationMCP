package com.sgm.navi.scene.ui.navi.manager;

import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * @author lww
 * @date 2025/3/20
 */
public class NaviSceneManager implements INaviSceneEvent {
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_MANAGER;
    /*** 所有的Scene **/
    private final ConcurrentMap<NaviSceneId, NaviSceneBase> sceneViewList;
    /*** 正在显示的Scene **/
    private final CopyOnWriteArrayList<NaviSceneBase> showSceneList;
    /*** 被隐藏的Scene **/
    private final CopyOnWriteArrayList<NaviSceneBase> hideSceneList;
    private final Runnable mRunnable;
    private boolean mIsCanAddScene;
    private boolean mIsForbidHandingCard; // 是否禁止悬挂卡展示

    private NaviSceneManager() {
        sceneViewList = new ConcurrentHashMap<>();
        showSceneList = new CopyOnWriteArrayList<>();
        hideSceneList = new CopyOnWriteArrayList<>();
        mRunnable = new Runnable() {
            @Override
            public void run() {
                restoreList();
            }
        };
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
        if (NaviSceneBase.SCENE_STATE_HIDE == sceneBase.getSceneState() ||
                NaviSceneBase.SCENE_STATE_CLOSE == sceneBase.getSceneState()) {
            return;
        }
        Logger.i(TAG, "onHideScene Name -> " , sceneBase.getSceneName());
        hideScene(sceneBase);
    }

    private void onShowScene(NaviSceneId cardId, SceneInfo info) {
            NaviSceneBase newSceneBase = getSceneById(cardId);
            if (!ConvertUtils.isEmpty(newSceneBase)) { // 先判断集合中是否已经包含该场景
                newSceneBase.setNeedShow(true);
                if (ConvertUtils.isEmpty(showSceneList)) { // 如果showSceneList为空则直接展示
                    Logger.i(TAG, "没有任何卡片在显示，直接展示新卡片", "newSceneBase:", newSceneBase.getSceneName());
                    showScene(newSceneBase, info);
                } else {
                    if (NaviSceneBase.SCENE_STATE_SHOW == newSceneBase.getSceneState()) { // 正在展示中不进行任何操作
                        Logger.i(TAG, "current ", newSceneBase.getSceneName(), " is show");
                    } else { // 没在展示，则根据碰撞规则展示Scene
                        ArrayList<NaviSceneBase> temporaryList = new ArrayList<>(showSceneList);
                        int showSceneListSize = temporaryList.size(); // 有人知道为什么要用size，而不是直接用showSceneList.size吗， 动脑筋猜一猜
                        Logger.d(TAG, "当前展示的Scene集合长度:", showSceneListSize, " 新卡:Name:", newSceneBase.getSceneName(),
                                " State:", newSceneBase.getSceneState(), " Id:", cardId.ordinal());
                        for (int i = 0; i < showSceneListSize; i++) {// 遍历所有显示的Scene，在根据定制的规则来判定正在显示的Scene是否需要隐藏或关闭
                            if (i >= temporaryList.size()) {
                                Logger.e(TAG, "ArrayIndexOutOfBoundsException", "i:", i, "size:", temporaryList.size());
                                break;
                            }
                            NaviSceneBase oldSceneView = temporaryList.get(i);
                            int sceneRule = getSceneRule(oldSceneView.getSceneId(), cardId);
                            if (!newSceneBase.isNeedShow()) {
                                Logger.i(TAG, "新卡片被关闭，不再需要展示", "newSceneBase:", newSceneBase.getSceneName());
                                return;
                            }
                            switch (sceneRule) {
                                case NaviSceneRule.SCENE_SHOW_AND_SHOW -> {
                                    if (Logger.openLog) {
                                        Logger.i(TAG, "双卡同时显示", "旧卡:", oldSceneView.getSceneName());
                                    }
                                    showScene(newSceneBase, info);
                                }
                                case NaviSceneRule.SCENE_SHOW_AND_CLOSE -> {
                                    if (Logger.openLog) {
                                        Logger.i(TAG, "旧卡显示,新卡关闭", "旧卡:", oldSceneView.getSceneName());
                                    }
                                    closeScene(newSceneBase);
                                    return; // 有人知道为什么要用return吗(新卡已经关闭了，后续不需要再处理了)
                                }
                                case NaviSceneRule.SCENE_SHOW_AND_HIDE -> {
                                    if (Logger.openLog) {
                                        Logger.i(TAG, "旧卡显示,新卡隐藏", "旧卡:" + oldSceneView.getSceneName());
                                    }
                                    hideScene(newSceneBase);
                                    return; // 有人知道为什么要用return吗(新卡已经隐藏了，后续不需要再处理了)
                                }
                                case NaviSceneRule.SCENE_HIDE_AND_SHOW -> {
                                    if (Logger.openLog) {
                                        Logger.i(TAG, "旧卡隐藏,新卡显示", "旧卡:", oldSceneView.getSceneName());
                                    }
                                    showScene(newSceneBase, info);
                                    hideScene(oldSceneView);
                                }
                                case NaviSceneRule.SCENE_CLOSE_AND_SHOW -> {
                                    if (Logger.openLog) {
                                        Logger.i(TAG, "旧卡关闭,新卡显示", "旧卡:" + oldSceneView.getSceneName());
                                    }
                                    showScene(newSceneBase, info);
                                    closeScene(oldSceneView);
                                    checkSceneReset(oldSceneView.getSceneId());
                                }
                                default -> {
                                    if (Logger.openLog) {
                                        Logger.i(TAG, "不做任何处理", "旧卡:" + oldSceneView.getSceneName());
                                    }
                                }
                            }
                        }
                    }
                }
            } else { // 不包含该场景代表这个Scene不受规则控制
                Logger.i(TAG, "不包含此场景");
            }
    }

    private void onCloseScene(NaviSceneId cardId) {
        NaviSceneBase sceneBase = getSceneById(cardId);
        if (ConvertUtils.isEmpty(sceneBase)) {
            return;
        }
        sceneBase.setNeedShow(false);
        if (NaviSceneBase.SCENE_STATE_CLOSE == sceneBase.getSceneState()) {
            return;
        }
        Logger.i(TAG, "onCloseScene", "Name:" , sceneBase.getSceneName() , " State:" , sceneBase.getSceneState());
        closeScene(sceneBase);
    }

    private void hideScene(@Nullable NaviSceneBase sceneView) {
        if (sceneView == null) {
            Logger.e(TAG, "sceneView==null");
            return;
        }
        if (NaviSceneBase.SCENE_STATE_CLOSE == sceneView.getSceneState()
                || NaviSceneBase.SCENE_STATE_HIDE == sceneView.getSceneState()) {
            /*if (Logger.openLog) {
                Logger.d(TAG, "current sceneView is close 不做处理: " , sceneView.getSceneName());
            }*/
            return;
        }
        /*if (Logger.openLog) {
            Logger.d(TAG, "sceneView: " + sceneView.getSceneName(), " getSceneState:" + sceneView.getSceneState());
        }*/
        if (ConvertUtils.isContain(showSceneList, sceneView)) {
            ConvertUtils.remove(showSceneList, sceneView);
        }
        if (!hideSceneList.contains(sceneView)) {
            hideSceneList.add(sceneView);
        } else {
            Logger.i(TAG, "hideSceneList 已存在！：" , sceneView.getSceneId().name());
        }
        if (ThreadManager.getInstance().isMainThread()) {
            sceneView.hide();
        } else {
            ThreadManager.getInstance().postUi(sceneView::hide);
        }
    }

    private void showScene(@Nullable NaviSceneBase sceneView, SceneInfo info) {
        if (sceneView == null) {
            Logger.e(TAG, "sceneView==null");
            return;
        }
        if (NaviSceneBase.SCENE_STATE_SHOW == sceneView.getSceneState()) {
            /*if (Logger.openLog) {
                Logger.d(TAG, "current sceneView is show 不做处理: ", sceneView.getSceneName());
            }*/
            return;
        }
        /*if (Logger.openLog) {
            Logger.d(TAG, "showScene: " + sceneView.getSceneName(), " getSceneState:" + sceneView.getSceneState());
        }*/
        if (!showSceneList.contains(sceneView)) {
            showSceneList.add(sceneView);
        } else {
            Logger.i(TAG, "showSceneList 已存在！：" , sceneView.getSceneId().name());
        }
        if (ConvertUtils.isContain(hideSceneList, sceneView)) {
            ConvertUtils.remove(hideSceneList, sceneView);
        }
        if (ThreadManager.getInstance().isMainThread()) {
            if (info != SceneInfo.Invalid) {
                sceneView.show(info);
            } else {
                sceneView.show();
            }
        } else {
            ThreadManager.getInstance().postUi(() -> {
                if (info != SceneInfo.Invalid) {
                    sceneView.show(info);
                } else {
                    sceneView.show();
                }
            });
        }
    }

    private void closeScene(@Nullable NaviSceneBase sceneView) {
        if (sceneView == null) {
            Logger.i(TAG, "sceneView==null");
            return;
        }
        if (NaviSceneBase.SCENE_STATE_CLOSE == sceneView.getSceneState()) {
            //Logger.d(TAG, "current sceneView is close 不做处理: ", sceneView.getSceneName());
            return;
        }
        /*if (Logger.openLog) {
            Logger.d(TAG, "closeScene", "sceneView -> " + sceneView.getSceneName() + "," + sceneView.getSceneState());
        }*/
        if (ConvertUtils.isContain(hideSceneList, sceneView)) {
            ConvertUtils.remove(hideSceneList, sceneView);
        }
        if (ConvertUtils.isContain(showSceneList, sceneView)) {
            ConvertUtils.remove(showSceneList, sceneView);
        }
        if (ThreadManager.getInstance().isMainThread()) {
            sceneView.close();
        } else {
            ThreadManager.getInstance().postUi(sceneView::close);
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
            Logger.d(TAG, "getSceneName -> " , newSceneBase.getSceneName(), "getSceneState:" , newSceneBase.getSceneState());
            showScene(newSceneBase, SceneInfo.Invalid);
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
        Logger.d(TAG, "addNaviScene", "id -> " , id.name(), "naviScene：" , naviScene.getSceneName() ,
                " mIsCanAddScene -> " , mIsCanAddScene);
        // 因为launcher巡航等页面会复用navi的scene所以这里只能在navi页面起来后才能添加scene
        if (!mIsCanAddScene || naviScene.getCategory() != NumberUtils.NUM_1) {
            return;
        }
        if (ConvertUtils.isContain(sceneViewList, id)) {
            Logger.i("addNaviScene", "已存在！：" , naviScene.getSceneId().name());
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
        Logger.i("notifySceneStateChange", "type:" , type , " cardId:" , cardId);
        if (type == SceneStateChangeType.SceneCloseState) {//关闭Scene
            onCloseScene(cardId);
        } else if (type == SceneStateChangeType.SceneShowState) {//显示Scene
            onShowScene(cardId, SceneInfo.Invalid);
        } else if (type == SceneStateChangeType.SceneHideState) {//隐藏Scene
            onHideScene(cardId);
        }
    }

    @Override
    public void notifySceneStateChangeReset(SceneStateChangeType type, NaviSceneId cardId, boolean isReset) {
        Logger.i("notifySceneStateChangeReset", "type:", type, " cardId:", cardId, " isReset:", isReset);
        if (type == SceneStateChangeType.SceneCloseState) {//关闭Scene
            onCloseScene(cardId);
            if (isReset) {
                checkSceneReset(cardId);
            }
        } else if (type == SceneStateChangeType.SceneShowState) {//显示Scene
            onShowScene(cardId, SceneInfo.Invalid);
        } else if (type == SceneStateChangeType.SceneHideState) {//隐藏Scene
            onHideScene(cardId);
        }
    }

    @Override
    public void notifySceneStateChange(SceneStateChangeType type, NaviSceneId cardId, SceneInfo info) {
        Logger.i("notifySceneStateChange", "type:", type, " cardId:", cardId + " info:", info);
        if (type == SceneStateChangeType.SceneCloseState) {//关闭Scene
            checkSceneReset(cardId);
            onCloseScene(cardId);
        } else if (type == SceneStateChangeType.SceneShowState) {//显示Scene
            onShowScene(cardId, info);
        } else if (type == SceneStateChangeType.SceneHideState) {//隐藏Scene
            onHideScene(cardId);
        }
    }

    /**
     * 当前正在显示的Scene关闭后判断是否需要还原隐藏Scene
     *
     * @param cardId
     */
    private void checkSceneReset(NaviSceneId cardId) {
        switch (cardId) {
            case NAVI_SCENE_2D_CROSS, NAVI_SCENE_3D_CROSS -> {
                resetSceneByIds(NaviSceneRule.CROSS_RESET);
            }
            case NAVI_SCENE_VIA_POINT_LIST -> {
                resetSceneByIds(NaviSceneRule.VIA_LIST_RESET);
            }
            case NAVI_SAPA_DETAIL_INFO -> {
                resetSceneByIds(NaviSceneRule.SAPA_DETAIL_RESET);
            }
            case NAVI_SCENE_PREFERENCE -> {
                resetSceneByIds(NaviSceneRule.PREFERENCE_DETAIL_RESET);
            }
            case NAVI_SUSPEND_CARD_DETAIL -> {
                resetSceneByIds(NaviSceneRule.SUSPEND_DETAIL_RESET);
            }
            case NAVI_SCENE_CONTROL_MORE -> {
                resetSceneByIds(NaviSceneRule.CONTROL_MORE_RESET);
            }
        }
    }

    /**
     * 将隐藏的scene展示
     */
    @Override
    public void notifySceneReset() {
        if (!ConvertUtils.isEmpty(hideSceneList)) {
            removeHideSceneById(NaviSceneId.NAVI_SCENE_PREFERENCE);
            removeHideSceneById(NaviSceneId.NAVI_SCENE_CONTROL_MORE);
            if (Logger.openLog) {
                Logger.i(TAG, "hideSceneList:", hideSceneList.size());
            }
            if(ThreadManager.getInstance().isMainThread()){
                for (NaviSceneBase newScene : hideSceneList) {
                    onShowScene(newScene.getSceneId(), SceneInfo.Invalid);
                }
            } else {
                ThreadManager.getInstance().postUi(() -> {
                    for (NaviSceneBase newScene : hideSceneList) {
                        onShowScene(newScene.getSceneId(), SceneInfo.Invalid);
                    }
                });
            }
        }
    }

    public void resetSceneByIds(NaviSceneId[] ids) {
        if (!ConvertUtils.isEmpty(hideSceneList)) {
            Logger.i(TAG, "resetSceneByIds:", hideSceneList.size());
            if (ThreadManager.getInstance().isMainThread()) {
                for (NaviSceneBase newScene : hideSceneList) {
                    for (NaviSceneId id : ids) {
                        if (newScene.getSceneId() == id) {
                            onShowScene(newScene.getSceneId(), SceneInfo.Invalid);
                            break;
                        }
                    }
                }
            } else {
                ThreadManager.getInstance().postUi(() -> {
                    for (NaviSceneBase newScene : hideSceneList) {
                        for (NaviSceneId id : ids) {
                            if (newScene.getSceneId() == id) {
                                onShowScene(newScene.getSceneId(), SceneInfo.Invalid);
                                break;
                            }
                        }
                    }
                });
            }
        } else {
            Logger.i(TAG, "hideSceneList is empty");
        }
    }

    private void removeHideSceneById(NaviSceneId sceneId) {
        if (!ConvertUtils.isEmpty(hideSceneList)) {
            for (NaviSceneBase newScene : hideSceneList) {
                if (newScene.getSceneId() == sceneId) {
                    hideSceneList.remove(newScene);
                    break;
                }
            }
        }
    }

    /**
     * @param list
     * @param sceneId
     * @return
     */
    private boolean isContainSceneById(CopyOnWriteArrayList<NaviSceneBase> list, NaviSceneId sceneId) {
        if (!ConvertUtils.isEmpty(list)) {
            for (NaviSceneBase scene : list) {
                if (scene.getSceneId() == sceneId) {
                    return true;
                }
            }
        }
        return false;
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

    public void restoreList() {
        if (!ConvertUtils.isEmpty(sceneViewList) && sceneViewList.size() < NumberUtils.NUM_22) {
            ThreadManager.getInstance().removeHandleTask(mRunnable);
            ThreadManager.getInstance().postDelay(mRunnable, NumberUtils.NUM_100);
        }
        ConvertUtils.clear(showSceneList);
        ConvertUtils.clear(hideSceneList);
        for (Map.Entry<NaviSceneId, NaviSceneBase> entry : sceneViewList.entrySet()) {
            NaviSceneBase base = entry.getValue();
            if (base.getSceneState() == NaviSceneBase.SCENE_STATE_SHOW) {
                showSceneList.add(base);
            }
            if (base.getSceneState() == NaviSceneBase.SCENE_STATE_HIDE) {
                hideSceneList.add(base);
            }
        }
    }

    /**
     * 设置是否禁止悬挂卡展示
     *
     * @param forbidHandingCard true:禁止悬挂卡展示，false:允许悬挂卡展示
     */
    public void setForbidHandingCard(boolean forbidHandingCard) {
        Logger.i(TAG, "forbidHandingCard:", forbidHandingCard);
        mIsForbidHandingCard = forbidHandingCard;
    }

    public boolean isForbidHandingCard() {
        return mIsForbidHandingCard;
    }

    @Override
    public void onCreateSceneView() {
        Logger.i(TAG, "onCreateSceneView");
        mIsCanAddScene = true;
    }

    private static final class Helper {
        private static final NaviSceneManager sm = new NaviSceneManager();
    }
}