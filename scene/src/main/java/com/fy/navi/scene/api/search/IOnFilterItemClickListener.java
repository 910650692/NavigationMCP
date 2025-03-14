package com.fy.navi.scene.api.search;

import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchChildCategoryLocalInfo;

import java.util.List;

/**
 * @Author: baipeng0904
 * @Description: 搜索POi列表点击事件
 * @Date: 2019/1/16 17:03
 */
public interface IOnFilterItemClickListener {
    void onItemClick(int position);

    void onChildListExpandCollapse(List<SearchChildCategoryLocalInfo> childList, int position);
}
