package com.fy.navi.scene.api.search;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 搜索主页面点击事件接口
 * @CreateDate: $ $
 */
public interface ISceneConfirmCancelListener {
    /**
     * 点击确定
     */
    void onConfirm();

    /**
     * 点击取消
     */
    void onCancel();
}
