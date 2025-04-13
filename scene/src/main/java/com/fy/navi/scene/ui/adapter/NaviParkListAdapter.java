
package com.fy.navi.scene.ui.adapter;

import static android.view.View.GONE;
import static android.view.View.VISIBLE;

import android.annotation.SuppressLint;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.api.navi.INaviParkItemClickListener;
import com.fy.navi.scene.databinding.SceneNaviParkListItemBinding;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.navi.NaviParkingEntity;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.navi.OpenApiHelper;
import com.fy.navi.ui.view.SkinTextView;

import java.util.ArrayList;
import java.util.List;

public class NaviParkListAdapter extends RecyclerView.Adapter<NaviParkListAdapter.ResultHolder> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private final List<NaviParkingEntity> mList;
    private INaviParkItemClickListener onItemClickListener;
    private int mSelectIndex;

    public int getSelectIndex() {
        return mSelectIndex;
    }

    public void setOnItemClickListener(INaviParkItemClickListener listener) {
        onItemClickListener = listener;
    }

    public NaviParkListAdapter() {
        this.mList = new ArrayList<>();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void notifyList(List<NaviParkingEntity> list, int select) {
        Logger.d(TAG, "NaviAddViaAdapter notifyList " + list);
        if (ConvertUtils.isEmpty(list)) {
            return;
        }
        mSelectIndex = select;
        mList.clear();
        mList.addAll(list);
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public ResultHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        SceneNaviParkListItemBinding itemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()), R.layout.scene_navi_park_list_item, parent, false);
        return new ResultHolder(itemBinding);
    }

    @SuppressLint("SetTextI18n")
    @Override
    public void onBindViewHolder(@NonNull ResultHolder holder, @SuppressLint("RecyclerView") int position) {
        Logger.d(TAG, "NaviAddViaAdapter onBindViewHolder " + position + ",mSelectIndex= " + mSelectIndex);
        int powerType = OpenApiHelper.powerType();
        NaviParkingEntity naviParkingEntity = mList.get(position);
        List<ChargeInfo> chargeInfoList = naviParkingEntity.getChargeInfoList();
        // 如果是纯电或者混动并且有充电站信息就显示充电站信息，否则显示停车场车位信息
        boolean isShowChargeInfo = (powerType == 1 || powerType == 2) && !ConvertUtils.
                isEmpty(chargeInfoList);
        if (isShowChargeInfo) {
            holder.itemBinding.stvParkingNum.setVisibility(GONE);
            holder.itemBinding.sclParkingOrChargeInfo.setVisibility(VISIBLE);
            setChargeData(holder, chargeInfoList);
        } else {
            holder.itemBinding.stvParkingNum.setVisibility(VISIBLE);
            holder.itemBinding.sclParkingOrChargeInfo.setVisibility(GONE);
        }
        holder.itemBinding.setParkBean(naviParkingEntity);
//        holder.itemBinding.sivParkingEnd.setVisibility((mList.size() == 1 && naviParkingEntity.isEndPoi()) ? View.VISIBLE : View.GONE);
        holder.itemBinding.stvNum.setText(position + 1 + "");
        if (!ConvertUtils.isEmpty(naviParkingEntity.getTag())) {
            holder.itemBinding.stvParkingState.setTextColor((naviParkingEntity.getTag().equals(AppContext.getInstance().getMContext().getString(R.string.navi_recommend_parking_adequate))) ?
                    AppContext.getInstance().getMContext().getResources().getColor(R.color.navi_color_C73333_100) :
                    AppContext.getInstance().getMContext().getResources().getColor(R.color.navi_color_2461EA_100));
        }
        holder.itemBinding.getRoot().setOnClickListener(v -> {
            Logger.d(TAG, "NaviAddViaAdapter item click " + position);
            if (onItemClickListener != null && mSelectIndex != position) {
                onItemClickListener.onItemClick(position);
                mSelectIndex = position;
                notifyDataSetChanged();
            }
        });
        holder.itemBinding.svParkingNavi.setOnClickListener(v -> {
            if (onItemClickListener != null) {
                onItemClickListener.onNaviClick(position, mList.get(position));
            }
        });
        holder.itemBinding.getRoot().setBackgroundResource(position == mSelectIndex ? R.color.common_item_select_color : R.color.transparent);
    }

    @SuppressLint("SetTextI18n")
    private void setChargeData(ResultHolder holder, List<ChargeInfo> chargeInfoList) {
        int fastChargeFreeCount = 0;
        int fastChargeTotalCount = 0;
        int slowChargeFreeCount = 0;
        int slowChargeTotalCount = 0;
        for (ChargeInfo chargeInfo : chargeInfoList) {
            if (chargeInfo == null) {
                continue;
            }
            fastChargeFreeCount = fastChargeFreeCount + chargeInfo.getFast_free();
            fastChargeTotalCount = fastChargeTotalCount + chargeInfo.getFast_total();
            slowChargeFreeCount = slowChargeFreeCount + chargeInfo.getSlow_free();
            slowChargeTotalCount = slowChargeTotalCount + chargeInfo.getSlow_total();
        }
        holder.itemBinding.stvFastChargeCount.setText(fastChargeFreeCount + "");
        holder.itemBinding.stvFastChargeTotal.setText("/" + fastChargeTotalCount);
        holder.itemBinding.stvSlowChargeCount.setText(slowChargeFreeCount + "");
        holder.itemBinding.stvSlowChargeTotal.setText("/" + slowChargeTotalCount);
    }

    @Override
    public int getItemCount() {
        Logger.d(TAG, "NaviAddViaAdapter getItemCount " + mList.size());
        return mList.size();
    }

    public void notifyItemSelect(int index) {
        final int originSelectIndex = mSelectIndex;
        if (mSelectIndex != index) {
            mSelectIndex = index;
            notifyItemChanged(index);
            notifyItemChanged(originSelectIndex);
            if (!ConvertUtils.isNull(onItemClickListener)) {
                onItemClickListener.onItemClick(index);
            }
        }
    }

    public static class ResultHolder extends RecyclerView.ViewHolder {
        public SceneNaviParkListItemBinding itemBinding;

        public ResultHolder(SceneNaviParkListItemBinding resultItemBinding) {
            super(resultItemBinding.getRoot());
            this.itemBinding = resultItemBinding;
            this.itemBinding.setHolder(this);
        }
    }
}