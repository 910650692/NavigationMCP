package com.fy.navi.scene.api.search;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: EditText文本框内容变化事件
 * @Date: 2019/1/16 17:03
 */
public interface IClearEditTextListener {
    /**
     * 搜索POi列表点击事件
     * @param content 内容
     */
    void onEditTextChanged(String content);
}
