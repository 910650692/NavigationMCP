package com.sgm.navi.scene.api.search;

import com.sgm.navi.service.define.search.SearchChildCategoryLocalInfo;
import com.sgm.navi.service.define.search.SearchGrandChildCategoryLocalInfo;

import java.util.List;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 搜索POi列表点击事件
 * @Date: 2019/1/16 17:03
 */
public interface IOnFilterItemClickListener {
    /**
     * 点击事件
     * @param position 点击下标
     */
    void onItemClick(int position);

    /**
     * 子列表展开收起事件
     * @param childList 子列表
     * @param position 点击下标
     */
    void onChildListExpandCollapse(List<SearchGrandChildCategoryLocalInfo> childList, int position);
}
