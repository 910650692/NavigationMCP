package com.sgm.navi.scene.ui.navi.manager;

/**
 * 卡片内部状态通知到 NaviSceneManager
 * 界面显示关闭通知、卡片状态显隐通知、其他消息通知
 * @author sgm
 * @version $Revision.*$
 */
public interface INaviSceneEvent {
    enum SceneStateChangeType {
        SceneShowState,
        SceneHideState,
        SceneCloseState
    }

    enum SceneInfo {
        Invalid,      //无效数据
        TipUnlock     //消息卡片打开地锁
    }

    /**
     * @param type type
     * @param cardId cardId
     */
    void notifySceneStateChange(SceneStateChangeType type, NaviSceneId cardId);

    /**
     * @param type type
     * @param cardId cardId
     */
    void notifySceneStateChangeReset(SceneStateChangeType type, NaviSceneId cardId, boolean isReset);

    /**
     * @param type type
     * @param cardId cardId
     * @param info info
     */
    void notifySceneStateChange(SceneStateChangeType type, NaviSceneId cardId, SceneInfo info);

    /**
     * 将隐藏的scene展示
     */
    void notifySceneReset();

    void destroySceneView();

    void onCreateSceneView();
}
