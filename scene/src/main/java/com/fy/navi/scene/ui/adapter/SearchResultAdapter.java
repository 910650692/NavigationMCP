
package com.fy.navi.scene.ui.adapter;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.text.SpannableString;
import android.text.Spanned;
import android.text.TextUtils;
import android.text.style.ForegroundColorSpan;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.adapter.GasStationAdapter;
import com.fy.navi.scene.adapter.GridSpacingItemDecoration;
import com.fy.navi.scene.databinding.SearchResultItemBinding;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.ChildInfo;
import com.fy.navi.service.define.search.GasStationInfo;
import com.fy.navi.service.define.search.ParkingInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.search.SearchPackage;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SearchResultAdapter extends RecyclerView.Adapter<SearchResultAdapter.ResultHolder> {
    private SearchResultEntity searchResultEntity;
    private final List<PoiInfoEntity> poiEntities;
    private OnItemClickListener onItemClickListener;
    private final SearchPackage searchPackage;
    private PoiInfoEntity poiInfoEntity;
    private int homeCompanyType = -1;// 1:家 2:公司 3:常用地址 0:收藏夹 -1:都不是
    private int spanCount = 2;
    private int spacing = 16;
    private int horizontalSpacing = 16;

    public void setOnItemClickListener(OnItemClickListener listener) {
        onItemClickListener = listener;
    }

    public int getHomeCompanyType() {
        return homeCompanyType;
    }

    public void setHomeCompanyType(int homeCompanyType) {
        this.homeCompanyType = homeCompanyType;
    }

    public SearchResultAdapter() {
        this.poiEntities = new ArrayList<>();
        searchPackage = SearchPackage.getInstance();
    }

    public void clearList() {
        int size = poiEntities.size();
        poiEntities.clear();
        if (size > 0) {
            notifyItemRangeRemoved(0, size);
        }
    }

    public void notifyList(SearchResultEntity searchResultEntity) {
        this.searchResultEntity = searchResultEntity;
        List<PoiInfoEntity> newPoiList = searchResultEntity.getPoiList();
        int oldSize = poiEntities.size();
        int newSize = newPoiList.size();

        poiEntities.clear();
        poiEntities.addAll(newPoiList);

        if (oldSize == 0 && newSize > 0) {
            notifyItemRangeInserted(0, newSize);
        } else if (oldSize > 0 && newSize == 0) {
            notifyItemRangeRemoved(0, oldSize);
        } else if (oldSize > 0) {
            notifyItemRangeChanged(0, Math.min(oldSize, newSize));
            if (newSize > oldSize) {
                notifyItemRangeInserted(oldSize, newSize - oldSize);
            } else if (newSize < oldSize) {
                notifyItemRangeRemoved(newSize, oldSize - newSize);
            }
        }
    }

    @NonNull
    @Override
    public ResultHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        SearchResultItemBinding adapterSearchResultItemBinding = DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()), R.layout.search_result_item, parent, false);
        return new ResultHolder(adapterSearchResultItemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull ResultHolder holder, int position) {
        holder.resultItemBinding.setPoiBean(poiEntities.get(position));
        poiInfoEntity = poiEntities.get(position);
        if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_KEYWORD
                || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.AROUND_SEARCH
                || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.ALONG_WAY_SEARCH
                || (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_SUGGESTION
                && homeCompanyType != -1)) {
            holder.resultItemBinding.poiNum.setVisibility(View.VISIBLE);
            holder.resultItemBinding.poiIcon.setVisibility(View.GONE);
            holder.resultItemBinding.setLayoutPosition(String.valueOf(position + 1));
        } else {
            holder.resultItemBinding.poiNum.setVisibility(View.GONE);
            holder.resultItemBinding.poiIcon.setVisibility(View.VISIBLE);
        }

        if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_SUGGESTION) {
            holder.resultItemBinding.poiName.setText(matcherSearchTitle(AppContext.mContext.getResources().getColor(R.color.navi_blue_text), poiEntities.get(position).getName(), searchResultEntity.getKeyword()));

        }

        if (searchPackage.isAlongWaySearch()) {
            holder.resultItemBinding.textNavi.setText(R.string.st_along_way_point);
            holder.resultItemBinding.ivNaviIcon.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_basic_ic_add));

        } else {
            holder.resultItemBinding.textNavi.setText(R.string.st_go_here);
            holder.resultItemBinding.ivNaviIcon.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_basic_ic_navi));

        }

        holder.resultItemBinding.crlPoiDes.setOnClickListener(v -> {
            Logger.d(SEARCH_HMI_TAG, "poi click 详情");
            if (onItemClickListener != null) {
                onItemClickListener.onItemClick(position, poiEntities.get(position));
            }

        });

        if (homeCompanyType != -1) {
            holder.resultItemBinding.ivNaviIcon.setImageResource(R.drawable.img_addq_58);
            switch (homeCompanyType) {
                case 0:
                case 3:
                    holder.resultItemBinding.textNavi.setText(R.string.st_collect_add);
                    break;
                case 1:
                    holder.resultItemBinding.textNavi.setText(R.string.st_home);
                    break;
                case 2:
                    holder.resultItemBinding.textNavi.setText(R.string.st_company);
                    break;
                default:
                    holder.resultItemBinding.textNavi.setText(R.string.st_go_here);
                    break;
            }
        }
        holder.resultItemBinding.poiToNavi.setOnClickListener(v -> {
            Logger.d(SEARCH_HMI_TAG, "poi click 算路|添加途经点");
            if (onItemClickListener != null) {
                onItemClickListener.onNaviClick(position, poiEntities.get(position));
            }

        });

        if (poiInfoEntity != null) {
            if (poiInfoEntity.getPointTypeCode() == null) {
                refreshNormalView();
                return;
            }
            int pointTypeCode = searchPackage.getPointTypeCode(poiInfoEntity.getPointTypeCode());
            Logger.d(SEARCH_HMI_TAG, "pointTypeCode is: " + pointTypeCode);
            switch (pointTypeCode) {
                case AutoMapConstant.PointTypeCode.GAS_STATION:
                    refreshGasStationView(holder);
                    break;
                case AutoMapConstant.PointTypeCode.CHARGING_STATION:
                    refreshChargeStationView(holder);
                    break;
                case AutoMapConstant.PointTypeCode.CAR_WASH:
                    refreshCarWashView(holder);
                    break;
                case AutoMapConstant.PointTypeCode.CATERING:
                    refreshCateringView(holder);
                    break;
                case AutoMapConstant.PointTypeCode.PARKING_LOT:
                    refreshParkingLotView(holder);
                    break;
                case AutoMapConstant.PointTypeCode.SERVICE_AREA:
                    refreshServiceAreaView(holder);
                    break;
                case AutoMapConstant.PointTypeCode.SCENIC_SPOT:
                    refreshScenicSpotView(holder);
                    break;
                case AutoMapConstant.PointTypeCode.OTHERS:
                default:
                    refreshNormalView();
                    break;
            }
        }

    }

    private void refreshNormalView() {
        Logger.d(SEARCH_HMI_TAG, "refreshNormalView do nothing");
    }

    private void refreshScenicSpotView(ResultHolder resultHolder) {
        if (poiInfoEntity.getAverageCost() == -1) {
            resultHolder.resultItemBinding.scenePoiItemScenicSpotView.poiScenicSpotPrice.setVisibility(View.GONE);
            resultHolder.resultItemBinding.scenePoiItemScenicSpotView.poiScenicSpotPriceIcon.setVisibility(View.GONE);
        } else {
            resultHolder.resultItemBinding.scenePoiItemScenicSpotView.poiScenicSpotPrice.setText(String.valueOf(poiInfoEntity.getAverageCost()));
        }

        List<ChildInfo> childInfoList = poiInfoEntity.getChildInfoList();
        PoiDetailsScenicChildAdapter scenicChildAdapter = new PoiDetailsScenicChildAdapter();
        if (childInfoList != null && !childInfoList.isEmpty()) {
            scenicChildAdapter.setChildInfoList(childInfoList);
            resultHolder.resultItemBinding.scenePoiItemScenicSpotView.poiScenicSpotChildList.setLayoutManager(new GridLayoutManager(resultHolder.resultItemBinding.getRoot().getContext(), spanCount));
            resultHolder.resultItemBinding.scenePoiItemScenicSpotView.poiScenicSpotChildList.addItemDecoration(new GridSpacingItemDecoration(resultHolder.resultItemBinding.getRoot().getContext(),spanCount, spacing, spacing, false));
            resultHolder.resultItemBinding.scenePoiItemScenicSpotView.poiScenicSpotChildList.setAdapter(scenicChildAdapter);
        }
        resultHolder.resultItemBinding.scenePoiItemScenicSpotView.getRoot().setVisibility(View.VISIBLE);
    }

    private void refreshServiceAreaView(ResultHolder resultHolder) {
        Logger.d(SEARCH_HMI_TAG, "refreshServiceAreaView");
    }

    private void refreshParkingLotView(ResultHolder resultHolder) {
        Logger.d(SEARCH_HMI_TAG, "refreshParkingLotView");
        if (poiInfoEntity == null || poiInfoEntity.getParkingInfoList() == null || poiInfoEntity.getParkingInfoList().isEmpty()) {
            return;
        }
        ParkingInfo parkingInfo = poiInfoEntity.getParkingInfoList().get(0);
        String parkString = "";
        int spaceFree = parkingInfo.getSpaceFree();
        int spaceTotal = parkingInfo.getSpaceTotal();
        Logger.d(SEARCH_HMI_TAG, "spaceFree :" + spaceFree + " spaceTotal :" + spaceTotal);
        if (spaceFree == -1 && spaceTotal == -1) {
            resultHolder.resultItemBinding.scenePoiItemParkingServiceView.getRoot().setVisibility(View.GONE);
        } else if (spaceFree == -1) {
            parkString = resultHolder.resultItemBinding.getRoot().getContext().getString(R.string.parking_lot_total, spaceTotal);
            resultHolder.resultItemBinding.scenePoiItemParkingServiceView.getRoot().setVisibility(View.VISIBLE);
            resultHolder.resultItemBinding.scenePoiItemParkingServiceView.poiParkingFreeTotal.setText(parkString);
        } else {
            resultHolder.resultItemBinding.scenePoiItemParkingServiceView.getRoot().setVisibility(View.VISIBLE);
            parkString = resultHolder.resultItemBinding.getRoot().getContext().getString(R.string.parking_lot_status, spaceFree, spaceTotal);
            resultHolder.resultItemBinding.scenePoiItemParkingServiceView.poiParkingFreeTotal.setText(parkString);
        }
    }

    private void refreshCateringView(ResultHolder resultHolder) {
        Logger.d(SEARCH_HMI_TAG, "refreshCateringView" + poiInfoEntity.getAverageCost());
        if (poiInfoEntity.getAverageCost() == -1) {
            resultHolder.resultItemBinding.scenePoiItemCateringView.getRoot().setVisibility(View.GONE);
        } else {
            resultHolder.resultItemBinding.scenePoiItemCateringView.poiCateringPrice.setText(String.valueOf(poiInfoEntity.getAverageCost()));
            resultHolder.resultItemBinding.scenePoiItemCateringView.getRoot().setVisibility(View.VISIBLE);
        }
    }

    private void refreshCarWashView(ResultHolder resultHolder) {
        Logger.d(SEARCH_HMI_TAG, "refreshCarWashView");
    }

    private void refreshChargeStationView(ResultHolder resultHolder) {
        Logger.d(SEARCH_HMI_TAG, "refreshChargeStationView");
        List<ChargeInfo> chargeInfos = poiInfoEntity.getChargeInfoList();
        ChargeInfo chargeInfo = chargeInfos.get(0);
        String fastFree = chargeInfo.getFast_free() == 0 ? "--" : chargeInfo.getFast_free() + "";
        String fastTotal = chargeInfo.getFast_total() == 0 ? "" : "/" + chargeInfo.getFast_total();
        resultHolder.resultItemBinding.scenePoiItemChargeView.poiChargeFastFree.setText(fastFree);
        if (!ConvertUtils.isEmpty(fastTotal)) {
            resultHolder.resultItemBinding.scenePoiItemChargeView.poiChargeFastTotal.setText(fastTotal);
        } else {
            resultHolder.resultItemBinding.scenePoiItemChargeView.poiChargeFastTotal.setVisibility(View.GONE);
        }

        String slowFree = chargeInfo.getSlow_free() == 0 ? "--" : chargeInfo.getSlow_free() + "";
        String slowTotal = chargeInfo.getSlow_total() == 0 ? "" : "/" + chargeInfo.getSlow_total();
        resultHolder.resultItemBinding.scenePoiItemChargeView.poiChargeSlowFree.setText(slowFree);
        if (!ConvertUtils.isEmpty(slowTotal)) {
            resultHolder.resultItemBinding.scenePoiItemChargeView.poiChargeSlowTotal.setText(fastTotal);
        } else {
            resultHolder.resultItemBinding.scenePoiItemChargeView.poiChargeSlowTotal.setVisibility(View.GONE);
        }
        resultHolder.resultItemBinding.scenePoiItemChargeView.poiChargePrice.setText(resultHolder.resultItemBinding.getRoot().getContext().getString
                (R.string.charge_price, chargeInfo.getCurrentElePrice()));
        resultHolder.resultItemBinding.scenePoiItemChargeView.getRoot().setVisibility(View.VISIBLE);

    }

    private void refreshGasStationView(ResultHolder resultHolder) {
        Logger.d(SEARCH_HMI_TAG, "refreshGasStationView");
        List<GasStationInfo> gasStationInfos = poiInfoEntity.getStationList();
        for (GasStationInfo gasStationInfo : gasStationInfos) {
            if (!gasStationInfo.getPrice().contains("升")) {
                gasStationInfo.setPrice(resultHolder.resultItemBinding.getRoot().getContext().getString(R.string.oil_price, gasStationInfo.getPrice()));
            }
        }
        GasStationAdapter gasStationAdapter = new GasStationAdapter();
        //根据UE只显示前两个油品种类
        if (!ConvertUtils.isEmpty(gasStationInfos) && gasStationInfos.size() > 2) {
            // 截取前两个元素
            List<GasStationInfo> subList = gasStationInfos.subList(0, 2);
            // 使用ArrayList构造函数创建新的列表
            ArrayList<GasStationInfo> newList = new ArrayList<>(subList);
            gasStationAdapter.setGasStationList(newList);
        } else {
            gasStationAdapter.setGasStationList(gasStationInfos);
        }
        resultHolder.resultItemBinding.scenePoiItemGasView.getRoot().setVisibility(View.VISIBLE);
        resultHolder.resultItemBinding.scenePoiItemGasView.poiGasOilList.setLayoutManager(new GridLayoutManager(resultHolder.resultItemBinding.getRoot().getContext(), spanCount));
        resultHolder.resultItemBinding.scenePoiItemGasView.poiGasOilList.setAdapter(gasStationAdapter);

    }


    @Override
    public int getItemCount() {
        return poiEntities.size();
    }

    public static class ResultHolder extends RecyclerView.ViewHolder {
        public SearchResultItemBinding resultItemBinding;

        public ResultHolder(SearchResultItemBinding resultItemBinding) {
            super(resultItemBinding.getRoot());
            this.resultItemBinding = resultItemBinding;
            this.resultItemBinding.setHolder(this);
        }
    }

    /**
     * 高亮显示文本
     *
     * @param color   需要高亮显示的颜色
     * @param text    显示的文字
     * @param keyword 搜索关键词
     * @return SpannableString
     */
    private SpannableString matcherSearchTitle(int color, String text, String keyword) {
        SpannableString spannableString = new SpannableString(text);
        if (null != keyword && !TextUtils.isEmpty(keyword)) {
            try {
                Pattern pattern = Pattern.compile(keyword);
                Matcher matcher = pattern.matcher(spannableString);
                while (matcher.find()) {
                    int start = matcher.start();
                    int end = matcher.end();
                    spannableString.setSpan(new ForegroundColorSpan(color), start, end, Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
                }
            } catch (Exception ex) {
                Logger.d("matcherSearchTitle", "ex: " + ex.getMessage());
            }
        }
        return spannableString;
    }

    public interface OnItemClickListener {
        void onItemClick(int position, PoiInfoEntity poiInfoEntity);

        void onNaviClick(int position, PoiInfoEntity poiInfoEntity);
    }
}