package com.sgm.navi.scene.ui.navi.hangingcard;

import com.sgm.navi.service.define.search.PoiInfoEntity;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/19
 * Description: [在这里描述文件功能]
 */
public interface OnHandingCardItemClickListener {
    void onItemSelect(int position, PoiInfoEntity poiInfo, int searchType);

    void onNaviNow(int position);
}
