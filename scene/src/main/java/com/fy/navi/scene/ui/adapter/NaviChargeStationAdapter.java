package com.fy.navi.scene.ui.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.ItemCardChargeBinding;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;
import java.util.List;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/2
 * Description: [在这里描述文件功能]
 */
public class NaviChargeStationAdapter extends RecyclerView.Adapter<NaviChargeStationAdapter.ViewHolder> {
    private final ArrayList<PoiInfoEntity> mData = new ArrayList<>();
    private Context mContext;
    private int mSelectIndex = 0;
    private OnItemClickListener mItemClickListener;

    public NaviChargeStationAdapter(final Context context, ArrayList<PoiInfoEntity> newData) {
        this.mContext = context;
        this.mData.clear();
        this.mData.addAll(newData);
    }

    public void updateData(List<PoiInfoEntity> newData) {
        this.mData.clear();
        this.mData.addAll(newData);
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        ItemCardChargeBinding binding = DataBindingUtil.inflate(LayoutInflater.from(mContext), R.layout.item_card_charge, parent, false);
        return new ViewHolder(binding);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        final PoiInfoEntity poiInfo = mData.get(position);
        final ChargeInfo chargeInfo = ConvertUtils.isEmpty(poiInfo.getChargeInfoList()) ? null : poiInfo.getChargeInfoList().get(0);
        final ItemCardChargeBinding binding = holder.mItemCardChargeBinding;
        binding.tvRecLabel.setVisibility(position == 0 ? View.VISIBLE : View.GONE);
        binding.getRoot().setBackgroundResource(position == mSelectIndex ? R.color.common_item_select_color : R.color.transparent);
        binding.stvNum.setText(String.valueOf(position + 1));
        binding.tvTitle.setText(poiInfo.getName());
        binding.llQuick.setVisibility(chargeInfo.getFast_total() > 0 ? View.VISIBLE : View.GONE);
        binding.llSlow.setVisibility(chargeInfo.getSlow_total() > 0 ? View.VISIBLE : View.GONE);
        binding.tvSum.setVisibility(chargeInfo.getFast_total() + chargeInfo.getSlow_total() > 0 ? View.VISIBLE : View.GONE);
        if (!ConvertUtils.isNull(chargeInfo)) {
            binding.tvQuickFree.setText(String.valueOf(chargeInfo.getFast_free()));
            binding.tvQuickTotal.setText("/" + chargeInfo.getFast_total());
            binding.tvSlowFree.setText(String.valueOf(chargeInfo.getSlow_free()));
            binding.tvSlowTotal.setText("/" + chargeInfo.getSlow_total());
            binding.tvDistance.setText(poiInfo.getDistance());
            final int desc = isPlentiful(chargeInfo) ? R.string.yong_ji : R.string.chong_zu;
            binding.tvSum.setText(desc);
        }
        binding.getRoot().setOnClickListener(v -> {
            if (!ConvertUtils.isNull(mItemClickListener)) {
                if (position != mSelectIndex) {
                    mSelectIndex = position;
                    notifyDataSetChanged();
                }
                mItemClickListener.onItemSelect(mSelectIndex);
            }
        });
        binding.viewNaviNow.setOnClickListener(v -> {
            if (!ConvertUtils.isNull(mItemClickListener)) {
                mItemClickListener.navi(poiInfo);
            }
        });
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    public void setOnItemClickListener(OnItemClickListener onItemClickListener) {
        this.mItemClickListener = onItemClickListener;
    }

    public int getSelectIndex() {
        return mSelectIndex;
    }

    public List<PoiInfoEntity> getData() {
        return mData;
    }

    public void updateSelect(final PoiInfoEntity poiInfoEntity) {
        final int tmpIndex = mSelectIndex;
        final int index = mData.indexOf(poiInfoEntity);
        if (index >= 0 && mSelectIndex != index) {
            mSelectIndex = index;
            if (!ConvertUtils.isNull(mItemClickListener)) {
                mItemClickListener.onItemSelect(mSelectIndex);
                notifyItemChanged(tmpIndex);
                notifyItemChanged(mSelectIndex);
            }
        }
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        private ItemCardChargeBinding mItemCardChargeBinding;

        public ViewHolder(@NonNull ItemCardChargeBinding binding) {
            super(binding.getRoot());
            this.mItemCardChargeBinding = binding;
        }
    }

    public interface OnItemClickListener {
        /***
         * 立即导航
         * @param poiInfo
         */
        void navi(final PoiInfoEntity poiInfo);

        /***
         * 选中
         * @param index
         */
        void onItemSelect(final int index);
    }

    /***
     * UE-2.6-1
     * -充电位紧张：总充电位数<=30个，剩余充电位<30% ；总充电位数>30个，剩余充电位<10% 或 剩余充电位少于10个。
     * @return true 紧张 false 充足
     */
    private boolean isPlentiful(final ChargeInfo chargeInfo) {
        final int totalSize = chargeInfo.getFast_total() + chargeInfo.getSlow_total();
        final int freeTotal = chargeInfo.getFast_free() + chargeInfo.getSlow_free();
        return totalSize <= 30 || (totalSize > 30 && freeTotal < 10);
    }
}
